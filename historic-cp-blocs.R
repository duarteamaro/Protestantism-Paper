## historic-cp-blocs.R
##
## Same year-by-year model, with the five Cage-Piketty blocs as they come in
## the data (no regrouping of our own). Benchmark for historic-party-groups.R.

source("historic-setup.R")

blocs <- c("Gauche (G)" = "pvoteG", "Centre-gauche (CG)" = "pvoteCG", "Centre (C)" = "pvoteC",
           "Centre-droite (CD)" = "pvoteCD", "Droite (D)" = "pvoteD")

res_blocs <- map(blocs, by_election) |>
  list_rbind(names_to = "bloc") |>
  mutate(bloc = factor(bloc, levels = names(blocs)))

write_csv(res_blocs, "tables/coef_per_year_cp_blocs.csv")

p <- plot_by_election(res_blocs) +
  facet_wrap(~bloc, ncol = 1, scales = "free_y", axes = "all_x") +
  labs(x = NULL, y = "Coefficient of Share of Protestants (1839) on bloc vote share",
       caption = "Dep. FE + historical controls + log population; 95% CIs from HC1 SEs.")

ggsave("figures/coef_per_year_cp_blocs.png", p, width = 13, height = 13, dpi = 300)
