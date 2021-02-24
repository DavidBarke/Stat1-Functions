#' Quantile skewness
#'
#' @param x A numeric vector.
#' @param p Probability (less than 0.5)
#'
#' @export
quantile_skewness <- function(x, p = 0.25) {
  stopifnot(p <= 0.5)
  q <- quantile(x, c(p, 0.5, 1 - p))
  ((q[3] - q[2]) - (q[2] - q[1])) / (q[3] - q[1])
}
