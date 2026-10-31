## historic-cleavages.R
##
## How does the 1839 Protestant share fare next to proxies of the classic
## Lipset-Rokkan cleavages? Election by election, one joint model with dep FE,
## all variables standardized, for three outcomes:
##   - the Cage-Piketty left share (left + centre-left + half the centre)
##   - our republican right
##   - our anti-republican right (only elections where such a family ran)
## Proxies:
##   - Protestant minority: Protestants 1839
##   - state-church: refractory clergy 1791, Catholic private schooling 1894
##   - centre-periphery: French speakers 1886
##   - owner-worker: income relative to France (log); from 1962 also the
##     shares of workers, farmers and managers
##   - urban-rural: agglomeration population (log)
##   - education: literacy (1876-1946), higher education (from 1962)
## Importance of each cleavage: Shapley decomposition of the within-department
## R2, i.e. the part of the explained variance each one accounts for.

source("historic-setup.R")
source("conley.R")

outcomes <- tribble(
  ~outcome,    ~suffix,   ~name,                          ~description,
  "left",      "left",    "left share",                   "Left share (Cage-Piketty: left + centre-left + half the centre)",
  "p_right",   "right",   "republican right vote",        "Republican right",
  "p_antirep", "antirep", "anti-republican right vote",   "Anti-republican right"
)

proxies <- tribble(
  ~var,                   ~label,                         ~cleavage,
  "share_protestant",     "Protestants 1839",             "Protestant minority",
  "prefract1791",         "Refractory clergy 1791",       "State-church",
  "privecanton1894_prim", "Catholic schooling 1894",      "State-church",
  "french_1886",          "French speakers 1886",         "Centre-periphery",
  "log_income",           "Income (log, relative)",       "Owner-worker",
  "pouvr",                "Workers",                      "Owner-worker",
  "pagri",                "Farmers",                      "Owner-worker",
  "pcadr",                "Managers",                     "Owner-worker",
  "log_agglo",            "Agglomeration size (log)",     "Urban-rural",
  "palpha",               "Literacy",                     "Education",
  "psup",                 "Higher education",             "Education"
)

cleavage_cols <- c("Protestant minority" = "#762a83", "State-church" = "#e08214",
                   "Centre-periphery" = "#1b7837", "Owner-worker" = "#b2182b",
                   "Urban-rural" = "#2166ac", "Education" = "#7f7f7f")

panel <- panel |>
  mutate(left = pvoteG + pvoteCG + pvoteC / 2,
         log_income = log(revratio),
         log_agglo = log(popagglo))


# Shapley decomposition -------------------------------------------------------------

## R2 of the (dep-demeaned) outcome y on a set of proxies
r2 <- function(d, vars) {
  if (length(vars) == 0) return(0)
  fit <- lm.fit(as.matrix(d[vars]), d$y)
  1 - sum(fit$residuals^2) / sum(d$y^2)
}

## Shapley value of each cleavage: its marginal R2 averaged over all orderings
shapley <- function(d, groups) {
  k <- length(groups)
  sets <- map(0:(2^k - 1), \(i) names(groups)[bitwAnd(i, 2^(seq_len(k) - 1)) > 0])
  key <- \(s) paste0("set:", paste(s, collapse = "|"))
  fit <- set_names(map_dbl(sets, \(s) r2(d, unlist(groups[s]))), map_chr(sets, key))
  map_dbl(set_names(names(groups)), \(g) {
    keep(sets, \(s) !g %in% s) |>
      map_dbl(\(s) factorial(length(s)) * factorial(k - length(s) - 1) / factorial(k) *
                (fit[[key(intersect(names(groups), c(s, g)))]] - fit[[key(s)]])) |>
      sum()
  })
}


# one election ------------------------------------------------------------------------

## proxies available in the election (more than 80% of communes). CIs from
## HC1 and from Conley SEs with a 100 km cutoff.
cleavage_models <- function(data, outcome) {
  vars <- proxies$var[map_lgl(proxies$var, \(v) mean(!is.na(data[[v]])) > 0.8)]
  d <- data |>
    rename(y = all_of(outcome)) |>
    drop_na(all_of(c("y", "dep", vars, "latitude_centre", "longitude_centre"))) |>
    mutate(across(all_of(c("y", vars)), \(x) as.numeric(scale(x))))
  groups <- split(vars, proxies$cleavage[match(vars, proxies$var)])
  within <- d |> mutate(across(all_of(c("y", vars)), \(x) x - mean(x)), .by = dep)
  conley <- conley_se(lm(reformulate(c(vars, "dep"), "y"), data = d), d, 100, vars)
  list(
    coefs = lm_robust(reformulate(c(vars, "dep"), "y"), data = d, se_type = "HC1") |>
      tidy(conf.int = TRUE) |>
      filter(term %in% vars) |>
      dplyr::select(-outcome) |>
      mutate(conley_se = conley[term],
             conley.low = estimate - 1.96 * conley_se,
             conley.high = estimate + 1.96 * conley_se),
    shapley = enframe(shapley(within, groups), name = "cleavage", value = "r2"),
    n = nrow(d)
  )
}

by_outcome <- function(outcome) {
  panel |>
    filter(!is.na(.data[[outcome]])) |>
    filter(sd(.data[[outcome]]) > 0, .by = election) |>
    nest(.by = c(election, year, republic)) |>
    mutate(fit = map(data, \(d) cleavage_models(d, outcome)), outcome = outcome) |>
    dplyr::select(-data)
}

res <- map(outcomes$outcome, by_outcome) |> list_rbind()

coefs <- res |>
  mutate(coefs = map(fit, "coefs")) |>
  dplyr::select(-fit) |>
  unnest(coefs) |>
  left_join(proxies, by = c("term" = "var"))

shares <- res |>
  mutate(shapley = map(fit, "shapley"), n = map_int(fit, "n")) |>
  dplyr::select(-fit) |>
  unnest(shapley) |>
  mutate(share = 100 * r2 / sum(r2), .by = c(outcome, election))

write_csv(coefs, "tables/cleavages_coefficients.csv")
write_csv(shares, "tables/cleavages_shapley.csv")


# figures, one set per outcome ------------------------------------------------------

note <- "Dep. FE. Occupations and higher education from 1962, literacy 1876-1946."

plot_shapley <- function(d, y, ylab, caption) {
  x_pos <- d |> distinct(election, year) |> arrange(year, election) |> mutate(x = row_number())
  d |>
    left_join(x_pos, by = c("election", "year")) |>
    mutate(cleavage = factor(cleavage, levels = names(cleavage_cols))) |>
    ggplot(aes(x = x, y = .data[[y]], fill = cleavage, colour = cleavage)) +
    geom_vline(xintercept = x_pos$x[x_pos$election == "1962"] - 0.5, linetype = "dashed",
               colour = "grey40") +
    scale_fill_manual(values = cleavage_cols, name = NULL) +
    scale_colour_manual(values = cleavage_cols, name = NULL) +
    scale_x_continuous(breaks = x_pos$x, labels = x_pos$election) +
    labs(x = NULL, y = ylab, caption = caption) +
    theme_light(base_size = 12) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
          legend.position = "bottom")
}

pwalk(outcomes, \(outcome, suffix, name, description) {
  caption <- paste0(description, ". ", note)
  s <- shares |> filter(outcome == .env$outcome)

  p <- plot_shapley(s, "r2", "Within-department R2 (Shapley decomposition)", caption) +
    geom_col(width = 0.8, colour = NA)
  ggsave(paste0("figures/cleavages_shapley_", suffix, ".png"), p, width = 14, height = 7, dpi = 300)

  p <- plot_shapley(s, "share", "Share of the explained variance (%)", caption) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.8)
  ggsave(paste0("figures/cleavages_shapley_share_", suffix, ".png"), p, width = 14, height = 7,
         dpi = 300)

  d_coefs <- coefs |>
    filter(outcome == .env$outcome) |>
    mutate(label = factor(label, levels = proxies$label))
  ylab <- paste("Standardized coefficient on the", name, "(SD per SD)")

  p <- plot_by_election(d_coefs) +
    facet_wrap(~label, ncol = 3, scales = "free_y", axes = "all_x") +
    labs(x = NULL, y = ylab,
         caption = paste(caption, "All proxies in one model; 95% CIs from HC1 SEs."))
  ggsave(paste0("figures/cleavages_coefficients_", suffix, ".png"), p, width = 14, height = 13,
         dpi = 300)

  p <- plot_by_election(d_coefs, lo = "conley.low", hi = "conley.high") +
    facet_wrap(~label, ncol = 3, scales = "free_y", axes = "all_x") +
    labs(x = NULL, y = ylab,
         caption = paste(caption, "All proxies in one model; 95% CIs from Conley SEs (100 km)."))
  ggsave(paste0("figures/cleavages_coefficients_conley_", suffix, ".png"), p, width = 14,
         height = 13, dpi = 300)
})
