## historic-referendums.R
##
## The referendums published by Cage-Piketty, with the model of the elections
## (dep FE, historical controls, log population in the referendum year, HC1):
##   - yes share in 1946 (first constitution draft, rejected), 1992 (Maastricht)
##     and 2005 (European constitution)
##   - turnout in all five. The constitutions of 1793 and 1795 passed with 99%
##     yes; there only participation varies.
## Built by build-historic-panel.R (referendum_panel.rds).

source("historic-setup.R")

referendums <- tribble(
  ~year, ~referendum,
  1793,  "1793 Constitution of Year I",
  1795,  "1795 Constitution of Year III",
  1946,  "May 1946 Constitution (rejected)",
  1992,  "1992 Maastricht Treaty",
  2005,  "2005 European Constitution"
)

d_ref <- read_rds("referendum_panel.rds") |>
  left_join(referendums, by = "year") |>
  mutate(election = referendum, republic = "")

res_ref <- bind_rows(
  by_election("yes", d_ref |> filter(year >= 1946)) |> mutate(outcome = "Yes share"),
  by_election("turnout", d_ref) |> mutate(outcome = "Turnout")
) |>
  mutate(referendum = factor(election, levels = referendums$referendum))

write_csv(res_ref, "tables/coef_referendums.csv")

p <- res_ref |>
  pivot_longer(c(estimate, std_estimate), names_to = "type", values_to = "coef") |>
  mutate(lo = if_else(type == "estimate", conf.low, std_conf.low),
         hi = if_else(type == "estimate", conf.high, std_conf.high),
         type = if_else(type == "estimate", "Raw (pp per pp)", "Standardized (SD per SD)")) |>
  ggplot(aes(x = referendum, y = coef)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_linerange(aes(ymin = lo, ymax = hi), linewidth = 0.9, colour = "#762a83") +
  geom_point(size = 2.6, colour = "#762a83") +
  facet_grid(type ~ outcome, scales = "free") +
  labs(x = NULL, y = "Coefficient of Share of Protestants (1839)",
       caption = "Dep. FE + historical controls + log population; 95% CIs from HC1 SEs.") +
  theme_light(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.margin = margin(5, 5, 5, 40))

ggsave("figures/coef_referendums.png", p, width = 12, height = 8, dpi = 300)
