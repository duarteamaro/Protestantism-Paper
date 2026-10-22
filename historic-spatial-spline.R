## historic-spatial-spline.R
##
## The main models control for latitude and longitude linearly. Here a 2-D
## thin-plate spline in projected coordinates replaces them, with growing
## flexibility k. If the 1839 coefficient survives the smooth surface, the
## association is not just generic geography.
##
## Far-right vote in 2022, in 1902 and pooled over all elections with an
## anti-republican right (with election FE).

source("historic-setup.R")
library(mgcv)

panel <- panel |>
  mutate(x_km = (longitude_centre - mean(longitude_centre, na.rm = TRUE)) * 111.320 *
           cos(mean(latitude_centre, na.rm = TRUE) * pi / 180),
         y_km = (latitude_centre - mean(latitude_centre, na.rm = TRUE)) * 110.574)

controls <- c(setdiff(hist_controls, c("latitude_centre", "longitude_centre")), "log_pop")

## k = 0: the linear lat/lon of the main models
spline_coef <- function(data, k, fe) {
  rhs <- paste(c("share_protestant", fe, "dep", controls), collapse = " + ")
  if (k == 0) {
    fit <- lm(as.formula(paste("p_antirep ~", rhs, "+ latitude_centre + longitude_centre")),
              data = data)
    est <- coef(summary(fit))["share_protestant", ]
  } else {
    fit <- bam(as.formula(paste0("p_antirep ~ ", rhs, " + s(x_km, y_km, bs = 'tp', k = ", k, ")")),
               data = data, discrete = TRUE)
    est <- summary(fit)$p.table["share_protestant", ]
  }
  tibble(estimate = est[[1]], se = est[[2]], p = est[[4]])
}

samples <- list(
  "Far right 2022 (RN+REC)" = panel |> filter(election == "2022"),
  "Far right 1902 (Dreyfus era)" = panel |> filter(election == "1902"),
  "Far right pooled (all elections)" = panel
) |>
  map(\(d) d |> drop_na(all_of(c("p_antirep", "share_protestant", "dep", controls, "x_km"))))

res <- crossing(outcome = names(samples), k = c(0, 20, 50, 100, 200)) |>
  mutate(fit = map2(outcome, k, \(o, k) spline_coef(samples[[o]], k,
                                                    if (str_detect(o, "pooled")) "election"))) |>
  unnest(fit) |>
  mutate(outcome = factor(outcome, levels = names(samples)))

write_csv(res, "tables/spatial_spline_sweep.csv")

p <- res |>
  mutate(k_pos = if_else(k == 0, -20, k)) |>
  ggplot(aes(x = k_pos, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_linerange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                 linewidth = 0.8, colour = "#762a83") +
  geom_point(size = 2.6, colour = "#762a83") +
  facet_wrap(~outcome, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = c(-20, 20, 50, 100, 200),
                     labels = c("linear\nlat/lon", "20", "50", "100", "200")) +
  labs(x = "Spline flexibility k (2-D thin-plate surface in coordinates)",
       y = "Coefficient on 1839 Protestant share") +
  theme_light(base_size = 12)

ggsave("figures/spatial_spline_sweep.png", p, width = 10, height = 9, dpi = 300)
