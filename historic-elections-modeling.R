## historic-elections-modeling.R
##
## 1839 Protestant share and the vote for the right across the legislative
## elections 1848-2022:
##   - pvoteD: the Cage-Piketty "droite" bloc. Their own classification, whose
##     content changes over time (monarchists in the 19th century, Gaullists in
##     the 1970s, FN/RN after 1986).
##   - pvoteDCD: droite + centre-droite
##   - p_antirep: our anti-republican / far-right group (monarchists,
##     Bonapartists, nationalists, Poujadists, FN/RN, Reconquete ...)
## Saturated tables per Republic, then the year-by-year coefficients.

source("historic-setup.R")
library(modelsummary)

panel <- panel |> mutate(pvoteDCD = pvoteD + pvoteCD)


# saturated models per Republic ---------------------------------------------------

## no election FE when the outcome exists in a single election (Poujadists 1956)
saturated <- function(data, outcome) {
  base <- c("share_protestant", if (n_distinct(data$election) > 1) "election", "dep")
  list(
    "Dep FEs" = lm(reformulate(base, outcome), data = data),
    "Historical Controls" = lm(reformulate(c(base, hist_controls), outcome), data = data),
    "Hist. + Pop." = lm(reformulate(c(base, hist_controls, "log_pop"), outcome), data = data),
    "Hist. + Pop. (Original only)" = lm(reformulate(c(base, hist_controls, "log_pop"), outcome),
                                        data = data |> filter(category == "Original"))
  )
}

gof <- tribble(~raw, ~clean, ~fmt,
               "nobs", "N", 0,
               "r.squared", "R^2", 2,
               "adj.r.squared", "Adj. R^2", 2)

outcomes <- tribble(
  ~outcome,    ~tag,          ~title,
  "pvoteD",    "right",       "the right-bloc vote",
  "pvoteDCD",  "rightCD",     "the right + centre-right vote",
  "p_antirep", "reactionary", "anti-republican/far-right families"
)

tables <- crossing(republic = c("Second", "Third", "Fourth", "Fifth"), outcomes)

pwalk(tables, \(republic, outcome, tag, title) {
  data <- panel |> filter(republic == .env$republic, !is.na(.data[[outcome]]))
  modelsummary(saturated(data, outcome),
               vcov = ~code_insee,
               stars = TRUE,
               coef_map = c("share_protestant" = "Share of Protestants in 1839"),
               gof_map = gof,
               title = paste0("Protestant share (1839) and ", title, ", ", republic, " Republic"),
               output = paste0("tables/table_", tolower(republic), "_republic_", tag, ".tex"))
})


# year by year --------------------------------------------------------------------

res_right <- by_election("pvoteD")
res_reac <- by_election("p_antirep")

write_csv(res_right, "tables/coef_per_year_historic.csv")
write_csv(res_reac, "tables/coef_per_year_reactionary.csv")

caption <- "Dep. FE + historical controls + log population; 95% CIs from HC1 SEs."

p <- plot_by_election(res_right) +
  labs(x = NULL, y = "Coefficient of Share of Protestants (1839) on right-bloc vote",
       caption = caption)
ggsave("figures/coef_per_year_historic.png", p, width = 14, height = 7, dpi = 300)

p <- plot_by_election(res_right, "std_estimate", "std_conf.low", "std_conf.high") +
  labs(x = NULL, y = "Standardized effect on right-bloc vote (SD per SD)", caption = caption)
ggsave("figures/coef_per_year_historic_std.png", p, width = 14, height = 7, dpi = 300)

p <- plot_by_election(res_reac) +
  labs(x = NULL, y = "Coefficient of Share of Protestants (1839) on anti-republican/far-right vote",
       caption = caption)
ggsave("figures/coef_per_year_reactionary.png", p, width = 14, height = 7, dpi = 300)

p <- plot_by_election(res_reac, "std_estimate", "std_conf.low", "std_conf.high") +
  labs(x = NULL, y = "Standardized effect on anti-republican/far-right vote (SD per SD)",
       caption = caption)
ggsave("figures/coef_per_year_reactionary_std.png", p, width = 14, height = 7, dpi = 300)
