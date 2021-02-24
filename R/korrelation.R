#' Correlation coefficients
#'
#' Wrapper to [stats::cor].
#'
#' @param x,y Vectors.
#'
#' @name correlation_coefficients
NULL

#' @rdname correlation_coefficients
#'
#' @export
cor_pearson <- function(x, y) {
  cor(x, y, method = "pearson")
}

#' @rdname correlation_coefficients
#'
#' @export
cor_spearman <- function(x, y) {
  cor(x, y, method = "spearman")
}

#' @rdname correlation_coefficients
#'
#' @export
cor_kendall <- function(x, y) {
  cor(x, y, method = "kendall")
}
