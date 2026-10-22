## historic-kelly-noise.R
##
## Kelly (2019) placebo test: does the 1839 share predict the far-right vote
## better than spatial noise with the same smoothness?
##   - placebo regressors are Gaussian random fields with the nugget share of
##     the 1839 Protestant share and an exponential covariance. The range is
##     not stably estimated, so we report 50, 100, 150 and 200 km.
##   - the outcome is regressed on each placebo with dep FE and the controls
##   - kelly_p = share of placebos with |t| >= |t| of the real regressor
## Fields are simulated as a low-rank predictive process (1,200 knots) so that
## all communes can be used; t-statistics come from Frisch-Waugh-Lovell.

source("historic-setup.R")

set.seed(20260101)

controls <- c(hist_controls, "log_pop")

## nugget / (nugget + sill) of an exponential variogram, range by grid search
nugget_share <- function(x, y, z, breaks = seq(0, 300, by = 15)) {
  dist_mat <- as.matrix(dist(cbind(x, y)))
  upper <- upper.tri(dist_mat)
  semivar <- tibble(bin = cut(dist_mat[upper], breaks),
                    sv = 0.5 * outer(z, z, "-")[upper]^2) |>
    filter(!is.na(bin)) |>
    summarise(gamma = mean(sv), n = n(), .by = bin) |>
    filter(n > 50) |>
    mutate(mid = breaks[as.integer(bin)] + diff(breaks)[1] / 2)
  best <- tibble(range = seq(5, 150, by = 2.5)) |>
    mutate(fit = map(range, \(a) lm(gamma ~ I(1 - exp(-mid / a)), data = semivar)),
           c0 = map_dbl(fit, \(f) max(coef(f)[[1]], 0)),
           c1 = map_dbl(fit, \(f) coef(f)[[2]]),
           sse = map_dbl(fit, \(f) sum(resid(f)^2))) |>
    filter(c1 > 0) |>
    slice_min(sse, n = 1, with_ties = FALSE)
  best$c0 / (best$c0 + best$c1)
}

## t-statistic of `focal` once the fixed design has been partialled out
fwl_t <- function(qr_fixed, y_res, focal) {
  f_res <- qr.resid(qr_fixed, as.numeric(focal))
  b <- sum(f_res * y_res) / sum(f_res^2)
  sigma2 <- sum((y_res - b * f_res)^2) / (length(y_res) - qr_fixed$rank - 1)
  b / sqrt(sigma2 / sum(f_res^2))
}

## placebo t-statistics for one assumed range (km)
placebo_t <- function(d, qr_fixed, y_res, nugget, range, nsim = 1000, m = 1200) {
  knots <- sample(nrow(d), m)
  chol_knots <- chol(exp(-as.matrix(dist(cbind(d$x_km[knots], d$y_km[knots]))) / range) +
                       diag(1e-8, m))
  cov_nm <- sapply(knots, \(j) exp(-sqrt((d$x_km - d$x_km[j])^2 + (d$y_km - d$y_km[j])^2) / range))
  proj <- cov_nm %*% chol2inv(chol_knots) %*% t(chol_knots)
  nugget_sd <- sqrt(nugget / (1 - nugget))
  map_dbl(seq_len(nsim), \(i) fwl_t(qr_fixed, y_res, proj %*% rnorm(m) + nugget_sd * rnorm(nrow(d))))
}

kelly_test <- function(election, ranges = c(50, 100, 150, 200)) {
  d <- panel |>
    filter(election == .env$election) |>
    drop_na(all_of(c("p_antirep", "share_protestant", "dep", controls))) |>
    mutate(x_km = (longitude_centre - mean(longitude_centre)) * 111.320 *
             cos(mean(latitude_centre) * pi / 180),
           y_km = (latitude_centre - mean(latitude_centre)) * 110.574)
  qr_fixed <- qr(model.matrix(reformulate(c("dep", controls)), data = d))
  y_res <- qr.resid(qr_fixed, d$p_antirep)
  t_real <- fwl_t(qr_fixed, y_res, d$share_protestant)
  s <- sample(nrow(d), min(5000, nrow(d)))
  nugget <- nugget_share(d$x_km[s], d$y_km[s], d$share_protestant[s])
  tibble(assumed_range_km = ranges) |>
    mutate(t_placebo = map(assumed_range_km, \(a) placebo_t(d, qr_fixed, y_res, nugget, a)),
           placebo_t_sd = map_dbl(t_placebo, sd),
           kelly_p = map_dbl(t_placebo, \(t) mean(abs(t) >= abs(t_real))),
           election = election, n = nrow(d), t_real = t_real, nugget = nugget) |>
    dplyr::select(-t_placebo)
}

res <- bind_rows(
  kelly_test("2022") |> mutate(outcome = "Far right 2022 (RN+REC)"),
  kelly_test("1902") |> mutate(outcome = "Far right 1902 (Dreyfus era)")
)

write_csv(res, "tables/kelly_noise_test.csv")

p <- ggplot(res, aes(x = assumed_range_km, y = kelly_p, colour = outcome)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("Far right 2022 (RN+REC)" = "#2166ac",
                                 "Far right 1902 (Dreyfus era)" = "#762a83"), name = NULL) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Assumed spatial range of placebo fields (km)",
       y = "Share of spatial-noise placebos with |t| >= real |t|",
       caption = "Below the dashed line: the 1839 coefficient beats spatially matched noise at 5%.") +
  theme_light(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("figures/kelly_noise_test.png", p, width = 10, height = 6.5, dpi = 300)
