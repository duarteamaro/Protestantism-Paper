## church-proximity.R
##
## Does proximity to a present-day Protestant church absorb the association
## between the 1839 Protestant share and the RN/FN vote (legislatives
## 1993-2022)?
##
## Churches: the geolocated directory of the Federation protestante de France
## (scraped-churches/). A church today is a consequence of historical
## settlement, not a confounder, so adding it is a mediation exercise: if the
## 1839 coefficient shrinks, the legacy runs through surviving congregations;
## if not, through something more diffuse. The first stage (1839 share ->
## church proximity) is therefore estimated as well.
##
## Church sets:
##   EPUdF     Eglise protestante unie, the successor of the Reformed church
##             the 1839 survey counted (default)
##   mainline  EPUdF + UEPAL (Lutheran, all in Alsace-Moselle, not in the survey)
##   fpf_all   every FPF church, including evangelical and pentecostal ones
##
## SEs clustered by commune (vcov = ~code_insee; modelsummary(vcov = "robust",
## cluster = ...) silently returns HC3 instead).

library(tidyverse)
library(estimatr)
library(broom)
library(modelsummary)

election_years <- c(1993, 1997, 2002, 2007, 2012, 2017, 2022)
church_set <- "EPUdF"


# churches ----------------------------------------------------------------------

## some congregations are listed twice under two departements
churches <- read_csv("scraped-churches/churches_fpf_insee.csv", show_col_types = FALSE) |>
  filter(!is.na(lat), !is.na(lng), !is.na(insee_code), insee_code != "") |>
  mutate(insee_code = str_pad(insee_code, width = 5, pad = "0")) |>
  filter(!str_starts(insee_code, "97"), !str_starts(insee_code, "98")) |>
  distinct(title, address, lat, lng, .keep_all = TRUE)

church_sets <- list(
  EPUdF = churches |> filter(str_starts(union, "EPUdF")),
  mainline = churches |> filter(str_starts(union, "EPUdF") | str_starts(union, "UEPAL")),
  fpf_all = churches
)

geo <- read_csv("communes-france-2025.csv", show_col_types = FALSE) |>
  dplyr::select(code_insee, altitude_moyenne, latitude_centre, longitude_centre) |>
  filter(!is.na(latitude_centre), !is.na(longitude_centre))

haversine_km <- function(lat1, lon1, lat2, lon2) {
  p <- pi / 180
  a <- sin((lat2 - lat1) * p / 2)^2 + cos(lat1 * p) * cos(lat2 * p) * sin((lon2 - lon1) * p / 2)^2
  2 * 6371.0088 * asin(pmin(1, sqrt(a)))
}

## distance from each commune centroid to the nearest church, and counts
church_covariates <- function(ch) {
  nearest <- rep(Inf, nrow(geo))
  n10 <- n25 <- n50 <- 0
  for (j in seq_len(nrow(ch))) {
    d <- haversine_km(geo$latitude_centre, geo$longitude_centre, ch$lat[j], ch$lng[j])
    nearest <- pmin(nearest, d)
    n10 <- n10 + (d <= 10)
    n25 <- n25 + (d <= 25)
    n50 <- n50 + (d <= 50)
  }
  tibble(code_insee = geo$code_insee,
         dist_church_km = nearest,
         log_dist_church = log1p(nearest),
         church_in_commune = as.integer(geo$code_insee %in% ch$insee_code),
         n_church_10km = n10,
         n_church_25km = n25,
         n_church_50km = n50)
}

d_church <- church_sets |>
  imap(\(ch, set) church_covariates(ch) |> rename_with(\(v) paste0(v, "_", set), -code_insee)) |>
  reduce(full_join, by = "code_insee")

write_csv(d_church, "tables/church_proximity_communes.csv")

## the selected set without suffix
church_vars <- c("dist_church_km", "log_dist_church", "church_in_commune", "n_church_25km")
d_church <- d_church |>
  mutate(dist_church_km = .data[[paste0("dist_church_km_", church_set)]],
         log_dist_church = .data[[paste0("log_dist_church_", church_set)]],
         church_in_commune = .data[[paste0("church_in_commune_", church_set)]],
         n_church_25km = .data[[paste0("n_church_25km_", church_set)]])


# treatment, controls and RN vote ------------------------------------------------

d_prot <- read_csv("clean_protestant_1839.csv", show_col_types = FALSE) |>
  filter(!is.na(share_protestant)) |>
  dplyr::select(code_insee, dep, nomdep, share_protestant, category)

read_ag <- function(file, ...) {
  read_csv(file, show_col_types = FALSE) |>
    dplyr::select(code_insee = insee_com, ...) |>
    mutate(code_insee = str_pad(code_insee, width = 5, pad = "0")) |>
    distinct(code_insee, .keep_all = TRUE)
}

read_cp <- function(file, ...) {
  read_csv(file, show_col_types = FALSE) |>
    dplyr::select(code_insee = codecommune, ...) |>
    mutate(code_insee = str_pad(code_insee, width = 5, pad = "0"))
}

protests <- read_csv("gay/nicolas_events_all_FR_labels.csv", show_col_types = FALSE) |>
  count(code_insee = str_pad(insee_2021, width = 5, pad = "0"), name = "total_protests")

d_controls <- geo |>
  left_join(read_ag("albertus_gay/dist_cassini.csv", dist_cassini), by = "code_insee") |>
  left_join(read_ag("albertus_gay/distance_relays_1708.csv", HubDist), by = "code_insee") |>
  left_join(read_ag("albertus_gay/letter_posts_1710.csv", letter), by = "code_insee") |>
  left_join(read_ag("albertus_gay/marechaussee_1720.csv", brigade), by = "code_insee") |>
  left_join(protests, by = "code_insee") |>
  left_join(read_cp("cage_piketty/religiositecommunes1791.csv", pserment1791), by = "code_insee") |>
  left_join(read_cp("cage_piketty/alphabetisationcommunes.csv", pconjsign1686), by = "code_insee") |>
  left_join(read_cp("cage_piketty/revcommunes.csv", revratio1790), by = "code_insee") |>
  mutate(across(c(letter, brigade, total_protests), \(x) replace_na(x, 0)),
         pconjsign1686 = 100 * pconjsign1686,
         pserment1791 = 100 * pserment1791)

## contemporary controls as in code.Rmd (pchom and the CSP shares come from
## cspcommunes.csv, which is not in the folder; prsa only starts in 2017)
contemporary <- function(file, stem, scale = 1) {
  read_cp(file, all_of(paste0(stem, election_years))) |>
    pivot_longer(-code_insee, names_to = "year", values_to = stem) |>
    mutate(year = as.integer(str_sub(year, -4)),
           across(all_of(stem), \(x) x * scale))
}

d_cont <- list(
  contemporary("cage_piketty/revcommunes.csv", "revmoy"),
  contemporary("cage_piketty/revcommunes.csv", "pop"),
  contemporary("cage_piketty/etrangerscommunes.csv", "petranger", 100),
  contemporary("cage_piketty/capitalimmobiliercommunes.csv", "prixbien", 1 / 1000),
  contemporary("cage_piketty/diplomescommunes.csv", "psup", 100),
  contemporary("cage_piketty/diplomescommunes.csv", "pbac", 100)
) |>
  reduce(full_join, by = c("code_insee", "year"))

## first-round RN/FN share; Cage-Piketty store pvoix as a proportion
load_rn <- function(year) {
  raw <- read_csv(paste0("cage_piketty/leg", year, "comm.csv"), show_col_types = FALSE,
                  col_types = cols(codecommune = col_character()))
  rn <- if ("pvoixRN" %in% names(raw)) "pvoixRN" else "pvoixFN"
  raw |>
    transmute(code_insee = str_pad(codecommune, width = 5, pad = "0"),
              year = year,
              pvoixRN = 100 * as.numeric(.data[[rn]]))
}

panel <- map(election_years, load_rn) |>
  list_rbind() |>
  inner_join(d_prot, by = "code_insee") |>
  left_join(d_controls, by = "code_insee") |>
  left_join(d_church, by = "code_insee") |>
  left_join(d_cont, by = c("code_insee", "year")) |>
  filter(!is.na(pvoixRN)) |>
  mutate(year = factor(year))


# first stage -------------------------------------------------------------------

xsec <- panel |> distinct(code_insee, .keep_all = TRUE)

bin_tab <- xsec |>
  mutate(bin = cut(share_protestant, breaks = c(-Inf, 0, 1, 5, 10, 25, 50, Inf),
                   labels = c("0", "(0,1]", "(1,5]", "(5,10]", "(10,25]", "(25,50]", "(50,100]"))) |>
  summarise(n_communes = n(),
            median_dist_km = round(median(dist_church_km, na.rm = TRUE), 1),
            mean_dist_km = round(mean(dist_church_km, na.rm = TRUE), 1),
            pct_with_church = round(100 * mean(church_in_commune, na.rm = TRUE), 1),
            .by = bin) |>
  arrange(bin)

write_csv(bin_tab, "tables/church_distance_by_bin.csv")

p_bin <- ggplot(bin_tab, aes(x = bin, y = median_dist_km)) +
  geom_col(fill = "#762a83", alpha = 0.85) +
  geom_text(aes(label = paste0(median_dist_km, " km")), vjust = -0.4, size = 3.4, colour = "grey30") +
  labs(x = "Protestant share in 1839 (%)",
       y = paste("Median distance to nearest", church_set, "church (km)")) +
  theme_light(base_size = 13)

ggsave("figures/church_distance_by_protestant_bin.png", p_bin, width = 9, height = 5.5, dpi = 300)

hist_controls <- c("letter", "brigade", "dist_cassini", "HubDist", "revratio1790",
                   "total_protests", "pconjsign1686", "pserment1791",
                   "altitude_moyenne", "latitude_centre", "longitude_centre")
cont_controls <- c("revmoy", "pop", "petranger", "prixbien")

first_stage <- list(
  "Distance (km)" = lm(reformulate(c("share_protestant", "dep", hist_controls), "dist_church_km"), data = xsec),
  "log(1 + distance)" = lm(reformulate(c("share_protestant", "dep", hist_controls), "log_dist_church"), data = xsec),
  "Church in commune" = lm(reformulate(c("share_protestant", "dep", hist_controls), "church_in_commune"), data = xsec),
  "N churches <= 25 km" = lm(reformulate(c("share_protestant", "dep", hist_controls), "n_church_25km"), data = xsec)
)

gof <- tribble(~raw, ~clean, ~fmt,
               "nobs", "N", 0,
               "r.squared", "R^2", 3)

modelsummary(first_stage, vcov = "HC1", stars = TRUE,
             coef_map = c("share_protestant" = "Share of Protestants in 1839"),
             gof_map = gof,
             title = paste("First stage: 1839 Protestant share and proximity to a present-day",
                           church_set, "church"),
             output = "tables/church_firststage.tex")

first_stage |>
  map(\(m) tidy(m) |> filter(term == "share_protestant")) |>
  list_rbind(names_to = "outcome") |>
  write_csv("tables/church_firststage.csv")


# absorption ------------------------------------------------------------------------

specs <- list(
  "Dep FEs" = c("share_protestant", "year", "dep"),
  "Historical controls" = c("share_protestant", "year", "dep", hist_controls),
  "Contemp. controls" = c("share_protestant", "year", "dep", cont_controls),
  "Both controls" = c("share_protestant", "year", "dep", hist_controls, cont_controls),
  "Both + higher ed." = c("share_protestant", "year", "dep", hist_controls, cont_controls, "psup"),
  "Both + higher ed. + bac" = c("share_protestant", "year", "dep", hist_controls, cont_controls,
                                "psup", "pbac")
)

church_terms <- c("Baseline (no church)" = NA, "+ distance (km)" = "dist_church_km",
                  "+ log distance" = "log_dist_church", "+ church in commune" = "church_in_commune",
                  "+ N churches <= 25 km" = "n_church_25km")

fit_rn <- function(rhs, data = panel) {
  lm_robust(reformulate(rhs[!is.na(rhs)], "pvoixRN"), data = data,
            clusters = code_insee, se_type = "stata")
}

absorb <- crossing(spec = names(specs), church_term = names(church_terms)) |>
  mutate(fit = map2(spec, church_term, \(s, ct) fit_rn(c(specs[[s]], church_terms[[ct]]))),
         n = map_dbl(fit, \(m) m$nobs),
         coef = map(fit, \(m) tidy(m) |> filter(term %in% c("share_protestant", church_vars)))) |>
  dplyr::select(-fit) |>
  unnest(coef)

summary_tab <- absorb |>
  filter(term == "share_protestant") |>
  mutate(pct_change = 100 * (estimate / estimate[church_term == "Baseline (no church)"] - 1),
         .by = spec) |>
  dplyr::select(spec, church_term, estimate, std.error, p.value, pct_change, n)

write_csv(summary_tab, "tables/church_absorption.csv")

modelsummary(map(church_terms, \(ct) fit_rn(c(specs[["Both controls"]], ct))),
             stars = TRUE,
             coef_map = c("share_protestant" = "Share of Protestants in 1839",
                          "dist_church_km" = "Distance to nearest church (km)",
                          "log_dist_church" = "log(1 + distance to nearest church)",
                          "church_in_commune" = "Church in commune",
                          "n_church_25km" = "N churches within 25 km"),
             gof_map = gof,
             title = paste("RN/FN vote share, historical and contemporary controls,",
                           "adding proximity to a present-day", church_set, "church"),
             output = "tables/church_absorption.tex")


## the same with each church set, fully saturated specification
by_set <- crossing(set = names(church_sets), church_term = c(NA, church_vars)) |>
  mutate(fit = map2(set, church_term, \(s, ct)
                    fit_rn(c(specs[["Both + higher ed. + bac"]], if (!is.na(ct)) paste0(ct, "_", s)))),
         n = map_dbl(fit, \(m) m$nobs),
         coef = map(fit, \(m) tidy(m) |> filter(term == "share_protestant"))) |>
  dplyr::select(-fit) |>
  unnest(coef) |>
  mutate(pct_change = 100 * (estimate / estimate[is.na(church_term)] - 1), .by = set)

write_csv(by_set, "tables/church_absorption_by_set.csv")


## year by year, both sets of controls
by_year <- crossing(year = election_years, church_term = names(church_terms)) |>
  mutate(fit = map2(year, church_term, \(y, ct)
                    lm_robust(reformulate(discard(c(setdiff(specs[["Both controls"]], "year"),
                                                    church_terms[[ct]]), is.na), "pvoixRN"),
                              data = panel |> filter(year == y), se_type = "HC1")),
         coef = map(fit, \(m) tidy(m, conf.int = TRUE) |> filter(term == "share_protestant"))) |>
  dplyr::select(-fit) |>
  unnest(coef)

write_csv(by_year, "tables/church_by_year.csv")


# figure ------------------------------------------------------------------------------

p <- summary_tab |>
  mutate(spec = factor(spec, levels = names(specs)),
         church_term = factor(church_term, levels = names(church_terms))) |>
  ggplot(aes(x = spec, y = estimate, colour = church_term, shape = church_term)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_linerange(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
                 position = position_dodge(width = 0.6), linewidth = 0.7) +
  geom_point(position = position_dodge(width = 0.6), size = 2.6) +
  scale_colour_manual(values = c("#333333", "#2166ac", "#4393c3", "#762a83", "#1b7837"), name = NULL) +
  scale_shape_manual(values = c(16, 15, 17, 18, 8), name = NULL) +
  labs(x = NULL, y = "Coefficient on the 1839 Protestant share",
       caption = paste0("RN/FN first-round vote, 1993-2022. Church set: ", church_set,
                        ". 95% CIs clustered by commune.")) +
  theme_light(base_size = 12) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "bottom")

ggsave("figures/church_absorption.png", p, width = 11, height = 7, dpi = 300)
