## historic-party-families.R
##
## The year-by-year model of historic-party-groups.R for every Cage-Piketty
## party family on its own, one figure per Republic: a panel per family, points
## coloured by the family's Cage-Piketty bloc in that election. Families below
## 1% of the national vote in every election of a Republic are left out.

source("historic-setup.R")

dict <- read_csv("tables/party_family_dictionary.csv", show_col_types = FALSE)
families <- read_rds("historic_families.rds")

## DIV is split equally over the five blocs by Cage-Piketty
bloc_cols <- c("G" = "#b2182b", "CG" = "#f4a582", "C" = "#7f7f7f", "CD" = "#92c5de",
               "D" = "#2166ac", "Mixed" = "#bababa")

kept <- dict |>
  filter(max(natl_vote_share_pct) >= 1, .by = c(republic, family)) |>
  mutate(bloc = if_else(cp_bloc_weight < 0.5, "Mixed", cp_bloc))

## one family: its share joined to the panel, then the usual model per election
family_coefs <- function(fam) {
  data <- families |>
    filter(family == fam) |>
    semi_join(kept |> filter(family == fam), by = "election") |>
    inner_join(panel, by = c("code_insee", "election"))
  by_election("share", data) |> mutate(family = fam)
}

res_families <- unique(kept$family) |>
  map(family_coefs) |>
  list_rbind() |>
  inner_join(kept |> dplyr::select(election, family, label, bloc, natl_vote_share_pct),
             by = c("election", "family"))

write_csv(res_families, "tables/coef_per_year_party_families.csv")


# one figure per Republic ------------------------------------------------------------

## panels ordered from left to right by the family's median bloc
plot_republic <- function(rep) {
  res <- res_families |>
    filter(republic == rep) |>
    mutate(panel = paste0(family, ": ", label),
           election = fct_reorder(election, year),
           bloc_rank = match(bloc, names(bloc_cols))) |>
    mutate(panel = fct_reorder(panel, bloc_rank, .fun = median),
           bloc = factor(bloc, levels = names(bloc_cols)))
  n_panels <- n_distinct(res$panel)

  p <- ggplot(res, aes(x = election, y = std_estimate, colour = bloc)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_linerange(aes(ymin = std_conf.low, ymax = std_conf.high), linewidth = 0.7) +
    geom_point(size = 1.6) +
    facet_wrap(~panel, ncol = 5, scales = "free_y", axes = "all_x",
               labeller = label_wrap_gen(30)) +
    scale_colour_manual(values = bloc_cols, name = "Cage-Piketty bloc", drop = FALSE) +
    labs(x = NULL, y = "Standardized effect of 1839 Protestant share (SD per SD)",
         caption = paste(rep, "Republic. Dep. FE + historical controls + log population;",
                         "95% CIs from HC1 SEs. Families with at least 1% of the national vote.")) +
    theme_light(base_size = 11) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
          strip.text = element_text(size = 8), legend.position = "bottom")

  ggsave(paste0("figures/coef_per_year_families_", tolower(rep), ".png"), p,
         width = 15, height = 2.2 * ceiling(n_panels / 5) + 1.5, dpi = 300, limitsize = FALSE)
}

walk(c("Second", "Third", "Fourth", "Fifth"), plot_republic)
