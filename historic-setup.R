## historic-setup.R
##
## Sourced by the historic-*.R scripts: the panel from build-historic-panel.R,
## the control vector of code.Rmd, the year-by-year model and the election axis
## of the coefficient plots.

library(tidyverse)
library(estimatr)
library(broom)

panel <- read_rds("historic_panel.rds")

hist_controls <- c("letter", "brigade", "dist_cassini", "HubDist", "revratio1790",
                   "total_protests", "pconjsign1686", "pserment1791",
                   "altitude_moyenne", "latitude_centre", "longitude_centre")

group_vars <- c(
  "Communist/Extreme left"  = "p_communist",
  "Socialist left"          = "p_socialist",
  "Green/ecologist"         = "p_green",
  "Republican/Radical left" = "p_radical",
  "Centre republican"       = "p_centre",
  "Republican right"        = "p_right",
  "Anti-republican right"   = "p_antirep",
  "Other/Divers"            = "p_other"
)

## one regression per election: dep FE, historical controls and log population
## in the election year, HC1 SEs. The standardized coefficient (SD of the
## outcome per SD of the Protestant share) uses the SDs of the estimation sample.
by_election <- function(outcome) {
  rhs <- c("share_protestant", "dep", hist_controls, "log_pop")
  panel |>
    drop_na(all_of(c(outcome, rhs))) |>
    filter(n() >= 500, sd(.data[[outcome]]) > 0, .by = election) |>
    nest(.by = c(election, year, republic)) |>
    mutate(fit = map(data, \(d) lm_robust(reformulate(rhs, outcome), data = d, se_type = "HC1")),
           scale = map_dbl(data, \(d) sd(d$share_protestant) / sd(d[[outcome]])),
           coef = map(fit, \(m) tidy(m, conf.int = TRUE) |> filter(term == "share_protestant"))) |>
    dplyr::select(-data, -fit) |>
    unnest(coef) |>
    mutate(across(c(estimate, conf.low, conf.high), \(x) x * scale, .names = "std_{.col}"))
}

rep_cols <- c(Second = "#7f7f7f", Third = "#762a83", Fourth = "#1b7837", Fifth = "#2166ac")

## elections are evenly spaced on the x axis; events are placed by calendar
## year. Gold band = Dreyfus Affair, dotted lines = 1870-71, WWI and WWII.
plot_by_election <- function(res, y = "estimate", lo = "conf.low", hi = "conf.high") {
  x_pos <- res |> distinct(election, year) |> arrange(year, election) |> mutate(x = row_number())
  at_year <- \(yr) approx(x_pos$year, x_pos$x, xout = yr, rule = 2, ties = mean)$y
  bands <- res |>
    distinct(election, republic) |>
    left_join(x_pos, by = "election") |>
    summarise(xmin = min(x) - 0.5, xmax = max(x) + 0.5, .by = republic)

  res |>
    left_join(x_pos, by = c("election", "year")) |>
    mutate(republic = factor(republic, levels = names(rep_cols))) |>
    ggplot(aes(x = x, y = .data[[y]], colour = republic)) +
    geom_rect(data = bands, inherit.aes = FALSE, alpha = 0.08,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = republic)) +
    annotate("rect", xmin = at_year(1894), xmax = at_year(1906), ymin = -Inf, ymax = Inf,
             fill = "#e0a800", alpha = 0.15) +
    geom_vline(xintercept = at_year(c(1870.5, 1916, 1942)), linetype = "dotted",
               colour = "grey30", linewidth = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_linerange(aes(ymin = .data[[lo]], ymax = .data[[hi]]), linewidth = 0.7) +
    geom_point(size = 1.6) +
    scale_colour_manual(values = rep_cols, name = "Republic") +
    scale_fill_manual(values = rep_cols, guide = "none") +
    scale_x_continuous(breaks = x_pos$x, labels = x_pos$election) +
    theme_light(base_size = 12) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
          legend.position = "bottom")
}
