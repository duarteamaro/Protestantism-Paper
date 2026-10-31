## historic-presidential.R
##
## The presidential elections published by Cage-Piketty (1848, 1965-2022), with
## the model of the legislative elections (dep FE, historical controls, log
## population, HC1):
##   1. every candidate with at least 1% in the first round, and both run-off
##      candidates, election by election
##   2. is the association stronger when the Republic is at stake? Coded from
##      the line-up of the decisive round, before estimation: a Bonapartist
##      (1848) or far-right candidate (2002, 2017, 2022) against a republican
##      one, versus ordinary left-right run-offs (1969 has no left candidate).
##      Outcomes: the share of the right-most candidate of the decisive round,
##      and the change in turnout between the two rounds.
##   3. the far-right candidates of the first round, which also cover the
##      1960s-70s when no far-right party appears in the legislative files.

source("historic-setup.R")

candidates <- read_csv("tables/presidential_candidates.csv", show_col_types = FALSE) |>
  mutate(bloc = if_else(cp_bloc_weight < 0.5, "Mixed", cp_bloc))
pres_panel <- read_rds("presidential_panel.rds") |>
  mutate(election = as.character(year), turnout_change = turnout_t2 - turnout)
pres_votes <- read_rds("presidential_votes.rds")

decisive <- tribble(
  ~year, ~round, ~candidate,        ~line_up,
  1848,  1,      "BONAPARTE",       "Republic at stake",
  1965,  2,      "DEGAULLE",        "Left-right run-off",
  1969,  2,      "POMPIDOU",        "Right-centre run-off",
  1974,  2,      "GISCARDDESTAING", "Left-right run-off",
  1981,  2,      "GISCARDDESTAING", "Left-right run-off",
  1988,  2,      "CHIRAC",          "Left-right run-off",
  1995,  2,      "CHIRAC",          "Left-right run-off",
  2002,  2,      "LEPEN",           "Republic at stake",
  2007,  2,      "SARKOZY",         "Left-right run-off",
  2012,  2,      "SARKOZY",         "Left-right run-off",
  2017,  2,      "MLEPEN",          "Republic at stake",
  2022,  2,      "MLEPEN",          "Republic at stake"
)

far_right <- c("TIXIERVIGNANCOUR", "LEPEN", "MEGRET", "MLEPEN", "ZEMMOUR")

bloc_cols <- c("G" = "#b2182b", "CG" = "#f4a582", "C" = "#7f7f7f", "CD" = "#92c5de",
               "D" = "#2166ac", "Mixed" = "#bababa")
line_up_cols <- c("Republic at stake" = "#762a83", "Left-right run-off" = "#1b7837",
                  "Right-centre run-off" = "#bababa")
caption <- "Dep. FE + historical controls + log population; 95% CIs from HC1 SEs."


# every candidate -------------------------------------------------------------------

candidate_coefs <- function(year, round, candidate) {
  data <- pres_votes |>
    filter(year == .env$year, round == .env$round, candidate == .env$candidate) |>
    inner_join(pres_panel, by = c("code_insee", "year"))
  by_election("share", data) |> mutate(round = round, candidate = candidate)
}

runs <- bind_rows(
  candidates |> filter(natl_vote_share_pct >= 1) |> transmute(year, round = 1, candidate),
  pres_votes |> filter(round == 2) |> distinct(year, round, candidate)
)

res_candidates <- pmap(runs, candidate_coefs) |>
  list_rbind() |>
  left_join(candidates |> dplyr::select(year, candidate, bloc, natl_vote_share_pct),
            by = c("year", "candidate"))

write_csv(res_candidates, "tables/coef_presidential_candidates.csv")

## candidates ordered from left to right within each election
p <- res_candidates |>
  mutate(key = paste(year, round, candidate, sep = "_"),
         key = fct_reorder(key, match(bloc, names(bloc_cols)) * 10 + round),
         bloc = factor(bloc, levels = names(bloc_cols)),
         round = if_else(round == 1, "First round", "Run-off")) |>
  ggplot(aes(x = key, y = std_estimate, colour = bloc, shape = round)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_linerange(aes(ymin = std_conf.low, ymax = std_conf.high), linewidth = 0.7) +
  geom_point(size = 2) +
  facet_wrap(~year, ncol = 3, scales = "free") +
  scale_x_discrete(labels = \(x) str_remove(x, "^\\d{4}_\\d_")) +
  scale_colour_manual(values = bloc_cols, name = "Cage-Piketty bloc", drop = FALSE) +
  scale_shape_manual(values = c("First round" = 16, "Run-off" = 17), name = NULL) +
  labs(x = NULL, y = "Standardized effect of 1839 Protestant share (SD per SD)",
       caption = paste(caption, "First-round candidates with at least 1%.")) +
  theme_light(base_size = 11) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 7), legend.position = "bottom")

ggsave("figures/coef_presidential_candidates.png", p, width = 14, height = 14, dpi = 300)


# Republic at stake -----------------------------------------------------------------

res_decisive <- decisive |>
  left_join(res_candidates, by = c("year", "round", "candidate")) |>
  mutate(outcome = "Share of the right-most candidate, decisive round")

res_turnout <- by_election("turnout_change", pres_panel |> filter(year > 1848)) |>
  left_join(decisive |> dplyr::select(year, line_up, candidate), by = "year") |>
  mutate(outcome = "Turnout, run-off minus first round (pp)")

write_csv(bind_rows(res_decisive, res_turnout), "tables/coef_presidential_decisive.csv")

## pooled over the decisive rounds (1969 left out): outcome and treatment
## standardized within election, election x dep FE, SEs clustered by commune
pooled_data <- decisive |>
  filter(year != 1969) |>
  inner_join(pres_votes, by = c("year", "round", "candidate")) |>
  inner_join(pres_panel, by = c("code_insee", "year")) |>
  drop_na(all_of(c("share", "share_protestant", "dep", hist_controls, "log_pop"))) |>
  mutate(across(c(share, share_protestant), \(x) as.numeric(scale(x))),
         at_stake = line_up == "Republic at stake",
         election_dep = paste(year, dep),
         .by = year)

pooled <- lm_robust(reformulate(c("share_protestant * at_stake", hist_controls, "log_pop"), "share"),
                    fixed_effects = ~election_dep, data = pooled_data,
                    clusters = code_insee, se_type = "stata") |>
  tidy(conf.int = TRUE) |>
  filter(str_detect(term, "share_protestant"))

write_csv(pooled, "tables/presidential_pooled_test.csv")

p <- bind_rows(res_decisive, res_turnout) |>
  mutate(year = factor(year),
         label = str_to_title(str_replace_all(candidate, c("GISCARDDESTAING" = "Giscard",
                                                           "DEGAULLE" = "de Gaulle",
                                                           "MLEPEN" = "M. Le Pen",
                                                           "^LEPEN" = "J.-M. Le Pen")))) |>
  ggplot(aes(x = year, y = std_estimate, colour = line_up)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_linerange(aes(ymin = std_conf.low, ymax = std_conf.high), linewidth = 0.9) +
  geom_point(size = 2.6) +
  geom_text(aes(label = label, y = std_conf.high), vjust = -0.6, size = 2.8, show.legend = FALSE) +
  facet_wrap(~outcome, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = line_up_cols, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(x = NULL, y = "Standardized effect of 1839 Protestant share (SD per SD)",
       caption = paste(caption, "Republic at stake: Bonapartist or far-right candidate",
                       "in the decisive round (coded from the line-up).")) +
  theme_light(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("figures/coef_presidential_republic_at_stake.png", p, width = 12, height = 9, dpi = 300)


# far right in the first round ------------------------------------------------------

p <- res_candidates |>
  filter(round == 1, candidate %in% far_right) |>
  mutate(year = factor(year),
         candidate = str_to_title(str_replace_all(candidate, c("TIXIERVIGNANCOUR" = "Tixier-Vignancour",
                                                               "MLEPEN" = "M. Le Pen",
                                                               "^LEPEN" = "J.-M. Le Pen")))) |>
  ggplot(aes(x = year, y = std_estimate, colour = candidate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_linerange(aes(ymin = std_conf.low, ymax = std_conf.high), linewidth = 0.9,
                 position = position_dodge(width = 0.5)) +
  geom_point(size = 2.6, position = position_dodge(width = 0.5)) +
  labs(x = NULL, y = "Standardized effect of 1839 Protestant share (SD per SD)",
       colour = NULL, caption = paste(caption, "First round.")) +
  theme_light(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("figures/coef_presidential_far_right.png", p, width = 11, height = 6, dpi = 300)
