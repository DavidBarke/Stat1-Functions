#' Empricial Variance
#'
#' @param x A numeric vector.
#'
#' @export
var_n <- function(x) {
  n <- length(x)
  (n - 1) / n * var(x)
}

#' Empiricial Standard Deviance
#'
#' @param x A numeric vector.
#'
#' @export
sd_n <- function(x) {
  sqrt(var_n(x))
}

#' Empricial Covariance
#'
#' @param x,y A numeric vector.
#'
#' @export
cov_n <- function(x, y) {
  n <- length(x)
  (n - 1) / n * cov(x, y)
}
