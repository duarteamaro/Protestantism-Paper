## historic-turnout.R
##
## Turnout (votants / inscrits) as the outcome: year by year and pooled within
## each Republic (election FE, SEs clustered by commune).

source("historic-setup.R")

res_turnout <- by_election("turnout")
write_csv(res_turnout, "tables/turnout_by_year.csv")

pooled <- panel |>
  drop_na(all_of(c("turnout", "share_protestant", "dep", hist_controls, "log_pop"))) |>
  nest(.by = republic) |>
  mutate(fit = map(data, \(d) lm_robust(reformulate(c("share_protestant", "election", "dep",
                                                      hist_controls, "log_pop"), "turnout"),
                                        data = d, clusters = code_insee, se_type = "stata")),
         n = map_int(data, nrow),
         coef = map(fit, \(m) tidy(m, conf.int = TRUE) |> filter(term == "share_protestant"))) |>
  dplyr::select(republic, n, coef) |>
  unnest(coef)

write_csv(pooled, "tables/turnout_pooled.csv")

p <- plot_by_election(res_turnout) +
  labs(x = NULL, y = "Coefficient of 1839 Protestant share on turnout (%)",
       caption = "Dep. FE + historical controls + log population; 95% CIs from HC1 SEs.")

ggsave("figures/turnout_by_year.png", p, width = 14, height = 7, dpi = 300)
