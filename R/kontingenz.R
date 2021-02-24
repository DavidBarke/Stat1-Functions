#' Chi Squared Value
#'
#' @param x A matrix.
#'
#' @export
chi_square <- function(x) {
  n <- sum(x)
  h_i <- matrix(rowSums(x))
  h_j <- matrix(colSums(x))
  h_ij <- h_i %*% t(h_j) / n
  sum((x - h_ij)^2 / h_ij)
}
