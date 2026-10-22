## historic-party-groups.R
##
## Coefficient of the 1839 Protestant share on the vote of each of our eight
## party groups, election by election (1848-2022). Groups are defined in
## build-party-dictionary.R.

source("historic-setup.R")

res_groups <- map(group_vars, by_election) |>
  list_rbind(names_to = "group") |>
  mutate(group = factor(group, levels = names(group_vars)))

write_csv(res_groups, "tables/coef_per_year_party_groups.csv")

caption <- paste("Dep. FE + historical controls + log population; 95% CIs from HC1 SEs.",
                 "Gold band = Dreyfus Affair (1894-1906);",
                 "dotted lines = Franco-Prussian War (1870-71), WWI, WWII/Vichy.")

p_raw <- plot_by_election(res_groups) +
  facet_wrap(~group, ncol = 2, scales = "free_y", axes = "all_x") +
  labs(x = NULL, y = "Coefficient of Share of Protestants (1839)", caption = caption)

ggsave("figures/coef_per_year_party_groups.png", p_raw, width = 13, height = 11, dpi = 300)

p_std <- plot_by_election(res_groups, "std_estimate", "std_conf.low", "std_conf.high") +
  facet_wrap(~group, ncol = 2, scales = "free_y", axes = "all_x") +
  labs(x = NULL, y = "Standardized effect of 1839 Protestant share (SD per SD)",
       caption = caption)

ggsave("figures/coef_per_year_party_groups_std.png", p_std, width = 13, height = 11, dpi = 300)
