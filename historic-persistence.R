## historic-persistence.R
##
## Does the 1839 share predict the 2022 far-right vote (RN + Reconquete)
## directly, or through the commune's own voting history? We add the commune's
## anti-republican vote in the Third Republic (1876-1906 average) and the FN
## vote of 1988 to the 2022 model, on a common sample.
##
## Descriptive only: the historic vote and the 2022 vote are both downstream of
## the same long process.

source("historic-setup.R")
library(modelsummary)

third_rep <- c("1876", "1881", "1885", "1889", "1893", "1898", "1902", "1906")

past_vote <- panel |>
  filter(election %in% c(third_rep, "1988")) |>
  mutate(period = if_else(election == "1988", "fn1988", "reac_3rep")) |>
  summarise(vote = mean(p_antirep, na.rm = TRUE), .by = c(code_insee, period)) |>
  pivot_wider(names_from = period, values_from = vote)

d_2022 <- panel |>
  filter(election == "2022") |>
  left_join(past_vote, by = "code_insee") |>
  drop_na(all_of(c("p_antirep", "reac_3rep", "fn1988", "share_protestant", "dep",
                   hist_controls, "log_pop")))

base <- c("share_protestant", "dep", hist_controls, "log_pop")

models <- list(
  "RN 2022" = lm_robust(reformulate(base, "p_antirep"), data = d_2022, se_type = "HC1"),
  "+ 3rd-Rep reaction" = lm_robust(reformulate(c(base, "reac_3rep"), "p_antirep"),
                                   data = d_2022, se_type = "HC1"),
  "+ FN 1988" = lm_robust(reformulate(c(base, "fn1988"), "p_antirep"),
                          data = d_2022, se_type = "HC1"),
  "+ both" = lm_robust(reformulate(c(base, "reac_3rep", "fn1988"), "p_antirep"),
                       data = d_2022, se_type = "HC1")
)

modelsummary(models,
             coef_map = c("share_protestant" = "Share of Protestants 1839",
                          "reac_3rep" = "Anti-republican right, 3rd Rep. avg (1876-1906)",
                          "fn1988" = "FN vote 1988"),
             gof_map = c("nobs", "r.squared"),
             stars = TRUE,
             output = "tables/persistence_chain.tex")

absorbed <- tibble(model = names(models),
                   estimate = map_dbl(models, \(m) coef(m)[["share_protestant"]])) |>
  mutate(pct_absorbed = 100 * (1 - estimate / first(estimate)))

write_csv(absorbed, "tables/persistence_chain.csv")
