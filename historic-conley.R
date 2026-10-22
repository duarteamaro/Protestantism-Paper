## historic-conley.R
##
## Conley (1999) spatial standard errors for the historic regressions, at 50 and
## 100 km (Bartlett kernel), next to HC1:
##   - per election, for the right bloc, the anti-republican right and the
##     eight party groups
##   - pooled by Republic and by decade for the right bloc and the
##     anti-republican right: spatial correlation within each election plus
##     any correlation within a commune over time
##
## conleyreg builds the dense n x n distance matrix, which does not fit for
## ~28k communes, so the weights are built sparsely from the neighbours within
## the cutoff (dbscan::frNN). Matches conleyreg to 4 decimals on subsamples.
## Slow: the group part runs for a while.

source("historic-setup.R")
library(Matrix)
library(dbscan)

## Bartlett weights between communes less than `cutoff` km apart
## (equirectangular projection around the sample centre)
conley_weights <- function(lat, lon, cutoff) {
  xy <- cbind((lon - mean(lon)) * 111.320 * cos(mean(lat) * pi / 180),
              (lat - mean(lat)) * 110.574)
  nb <- frNN(xy, eps = cutoff)
  n <- length(lat)
  sparseMatrix(i = c(rep(seq_len(n), lengths(nb$id)), seq_len(n)),
               j = c(unlist(nb$id), seq_len(n)),
               x = c(1 - unlist(nb$dist) / cutoff, rep(1, n)),
               dims = c(n, n))
}

## sandwich with the spatial meat; `data` is the estimation sample of `fit`
conley_se <- function(fit, data, cutoff) {
  x <- model.matrix(fit)
  u <- x * residuals(fit)
  w <- conley_weights(data$latitude_centre, data$longitude_centre, cutoff)
  bread <- solve(crossprod(x))
  v <- bread %*% as.matrix(t(u) %*% (w %*% u)) %*% bread
  sqrt(diag(v))[["share_protestant"]]
}

## pooled panel: spatial meat within each election + serial meat within commune
conley_panel_se <- function(fit, data, cutoff) {
  x <- model.matrix(fit)
  u <- x * residuals(fit)
  rows <- split(seq_len(nrow(data)), data$election)
  spatial <- map(rows, \(r) {
    w <- conley_weights(data$latitude_centre[r], data$longitude_centre[r], cutoff)
    as.matrix(t(u[r, , drop = FALSE]) %*% (w %*% u[r, , drop = FALSE]))
  }) |>
    reduce(`+`)
  serial <- crossprod(rowsum(u, data$code_insee)) - crossprod(u)
  bread <- solve(crossprod(x))
  v <- bread %*% (spatial + serial) %*% bread
  sqrt(diag(v))[["share_protestant"]]
}

rhs <- c("share_protestant", "dep", hist_controls, "log_pop")


# per election --------------------------------------------------------------------

conley_by_election <- function(outcome) {
  panel |>
    drop_na(all_of(c(outcome, rhs))) |>
    filter(n() >= 500, sd(.data[[outcome]]) > 0, .by = election) |>
    nest(.by = c(election, year, republic)) |>
    mutate(fit = map(data, \(d) lm(reformulate(rhs, outcome), data = d)),
           robust = map(data, \(d) lm_robust(reformulate(rhs, outcome), data = d, se_type = "HC1")),
           estimate = map_dbl(fit, \(m) coef(m)[["share_protestant"]]),
           df = map_dbl(fit, df.residual),
           scale = map_dbl(data, \(d) sd(d$share_protestant) / sd(d[[outcome]])),
           HC1 = map_dbl(robust, \(m) m$std.error[["share_protestant"]]),
           `Conley 50km` = map2_dbl(fit, data, \(m, d) conley_se(m, d, 50)),
           `Conley 100km` = map2_dbl(fit, data, \(m, d) conley_se(m, d, 100))) |>
    dplyr::select(-data, -fit, -robust) |>
    pivot_longer(c(HC1, starts_with("Conley")), names_to = "spec", values_to = "se") |>
    mutate(outcome = outcome,
           conf.low = estimate - 1.96 * se,
           conf.high = estimate + 1.96 * se,
           p = 2 * pt(-abs(estimate / se), df))
}

outcomes <- c("Right bloc (pvoteD)" = "pvoteD", group_vars)

res_conley <- map(outcomes, conley_by_election) |>
  list_rbind(names_to = "outcome_label") |>
  mutate(across(c(estimate, conf.low, conf.high), \(x) x * scale, .names = "std_{.col}"))

write_csv(res_conley, "tables/conley_per_election.csv")


# pooled by Republic and by decade ------------------------------------------------

conley_pooled <- function(outcome, grouping) {
  panel |>
    mutate(decade = paste0(10 * floor(year / 10), "s")) |>
    drop_na(all_of(c(outcome, rhs))) |>
    nest(.by = all_of(grouping)) |>
    rename(group = 1) |>
    filter(map_int(data, nrow) >= 500) |>
    mutate(rhs_pooled = map(data, \(d) if (n_distinct(d$election) > 1) c(rhs, "election") else rhs),
           fit = map2(data, rhs_pooled, \(d, r) lm(reformulate(r, outcome), data = d)),
           cluster = map2(data, rhs_pooled, \(d, r) lm_robust(reformulate(r, outcome), data = d,
                                                               clusters = code_insee, se_type = "stata")),
           n = map_int(data, nrow),
           elections = map_int(data, \(d) n_distinct(d$election)),
           estimate = map_dbl(fit, \(m) coef(m)[["share_protestant"]]),
           se_cluster = map_dbl(cluster, \(m) m$std.error[["share_protestant"]]),
           se_conley = map2_dbl(fit, data, \(m, d) conley_panel_se(m, d, 100)),
           p_cluster = 2 * pnorm(-abs(estimate / se_cluster)),
           p_conley = 2 * pnorm(-abs(estimate / se_conley))) |>
    dplyr::select(group, n, elections, estimate, se_cluster, p_cluster, se_conley, p_conley) |>
    mutate(outcome = outcome, grouping = grouping)
}

pooled <- crossing(outcome = c("pvoteD", "p_antirep"), grouping = c("republic", "decade")) |>
  pmap(conley_pooled) |>
  list_rbind()

write_csv(pooled, "tables/conley_pooled.csv")


# figures -------------------------------------------------------------------------

## HC1 and Conley CIs side by side for each election
plot_conley <- function(res, y = "estimate", lo = "conf.low", hi = "conf.high") {
  plot_by_election(res, y, lo, hi) +
    aes(shape = spec, group = spec) +
    scale_shape_manual(values = c("HC1" = 16, "Conley 50km" = 15, "Conley 100km" = 17), name = NULL)
}

for (std in c(FALSE, TRUE)) {
  cols <- if (std) c("std_estimate", "std_conf.low", "std_conf.high") else c("estimate", "conf.low", "conf.high")
  ylab <- if (std) "Standardized effect (SD per SD)" else "Coefficient of 1839 Protestant share"
  suffix <- if (std) "_std" else ""

  p <- plot_conley(res_conley |> filter(outcome == "pvoteD"), cols[1], cols[2], cols[3]) +
    labs(x = NULL, y = ylab, caption = "Right bloc (pvoteD). HC1 vs Conley SEs at 50/100 km.")
  ggsave(paste0("figures/conley_per_election_right", suffix, ".png"), p, width = 14, height = 7, dpi = 300)

  p <- plot_conley(res_conley |> filter(outcome == "p_antirep"), cols[1], cols[2], cols[3]) +
    labs(x = NULL, y = ylab, caption = "Anti-republican right. HC1 vs Conley SEs at 50/100 km.")
  ggsave(paste0("figures/conley_per_election_reac", suffix, ".png"), p, width = 14, height = 7, dpi = 300)

  p <- plot_conley(res_conley |>
                     filter(outcome %in% group_vars) |>
                     mutate(outcome_label = factor(outcome_label, levels = names(group_vars))),
                   cols[1], cols[2], cols[3]) +
    facet_wrap(~outcome_label, ncol = 2, scales = "free_y", axes = "all_x") +
    labs(x = NULL, y = ylab, caption = "HC1 vs Conley SEs at 50/100 km.")
  ggsave(paste0("figures/conley_per_election_groups", suffix, ".png"), p, width = 13, height = 12, dpi = 300)
}
