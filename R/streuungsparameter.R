#' Empirical deviations
#'
#' @param x,y Numeric vectors.
#'
#' @name empirical_deviation
NULL

#' @rdname empirical_deviation
#'
#' @export
var_n <- function(x) {
  n <- length(x)
  (n - 1) / n * var(x)
}

#' @rdname empirical_deviation
#'
#' @export
sd_n <- function(x) {
  sqrt(var_n(x))
}

#' @rdname empirical_deviation
#'
#' @export
cov_n <- function(x, y) {
  n <- length(x)
  (n - 1) / n * cov(x, y)
}
