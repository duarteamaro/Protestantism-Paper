## conley.R
##
## Conley (1999) spatial standard errors with a Bartlett kernel, sourced by
## historic-conley.R and historic-cleavages.R.
##
## conleyreg builds the dense n x n distance matrix, which does not fit for
## ~28k communes, so the weights are built sparsely from the neighbours within
## the cutoff (dbscan::frNN). Matches conleyreg to 4 decimals on subsamples.

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
conley_se <- function(fit, data, cutoff, terms = "share_protestant") {
  x <- model.matrix(fit)
  u <- x * residuals(fit)
  w <- conley_weights(data$latitude_centre, data$longitude_centre, cutoff)
  bread <- solve(crossprod(x))
  v <- bread %*% as.matrix(t(u) %*% (w %*% u)) %*% bread
  sqrt(diag(v))[terms]
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
