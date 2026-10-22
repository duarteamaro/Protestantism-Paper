## historic-generality.R
##
## Is the association with the far-right vote a dose-response, or is it driven
## by a few strongholds (Gard, Ardeche, Lozere, Drome, Deux-Sevres)? Pooled
## anti-republican right vote, election + dep FE, historical controls, log
## population, SEs clustered by commune:
##   1. dropping communes above a ceiling of Protestant share
##   2. bins of Protestant share against communes without Protestants

source("historic-setup.R")

rhs <- c("election", "dep", hist_controls, "log_pop")

d_reac <- panel |>
  drop_na(all_of(c("p_antirep", "share_protestant", rhs))) |>
  mutate(bin = cut(share_protestant,
                   breaks = c(-Inf, 0, 1, 2, 5, 10, 25, 50, Inf),
                   labels = c("0", "(0,1]", "(1,2]", "(2,5]", "(5,10]", "(10,25]",
                              "(25,50]", "(50,100]")))


# ceilings ----------------------------------------------------------------------

trim <- tibble(ceiling = c(Inf, 50, 25, 10, 5, 3)) |>
  mutate(data = map(ceiling, \(cap) d_reac |> filter(share_protestant <= cap)),
         fit = map(data, \(d) lm_robust(reformulate(c("share_protestant", rhs), "p_antirep"),
                                        data = d, clusters = code_insee, se_type = "stata")),
         n_communes = map_int(data, \(d) n_distinct(d$code_insee)),
         coef = map(fit, \(m) tidy(m, conf.int = TRUE) |> filter(term == "share_protestant"))) |>
  dplyr::select(ceiling, n_communes, coef) |>
  unnest(coef)

write_csv(trim, "tables/generality_trim_sweep.csv")


# bins ----------------------------------------------------------------------------

bin_n <- d_reac |>
  distinct(code_insee, bin) |>
  count(bin, name = "n_communes")

bins <- lm_robust(reformulate(c("bin", rhs), "p_antirep"), data = d_reac,
                  clusters = code_insee, se_type = "stata") |>
  tidy(conf.int = TRUE) |>
  filter(str_starts(term, "bin")) |>
  mutate(bin = str_remove(term, "^bin")) |>
  bind_rows(tibble(bin = "0", estimate = 0, conf.low = 0, conf.high = 0)) |>
  mutate(bin = factor(bin, levels = levels(d_reac$bin))) |>
  left_join(bin_n, by = "bin") |>
  arrange(bin)

write_csv(bins, "tables/generality_dose_response.csv")

p <- ggplot(bins, aes(x = bin, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), linewidth = 0.9, colour = "#762a83") +
  geom_point(size = 3, colour = "#762a83") +
  geom_text(aes(label = scales::comma(n_communes)), vjust = -1.1, size = 3, colour = "grey40") +
  labs(x = "Protestant share in 1839 (%)",
       y = "Far-right vote vs. communes without Protestants (pp)",
       caption = paste("Pooled elections with an anti-republican right; election and dep. FE,",
                       "historical controls, log population; 95% CIs clustered by commune.",
                       "\nNumbers = communes per bin.")) +
  theme_light(base_size = 13)

ggsave("figures/generality_dose_response.png", p, width = 11, height = 6.5, dpi = 300)
