#' @export
harmonic_mean <- function(x) {
  if (any(x == 0)) return(0)
  1 / mean(1 / x)
}

#' @export
geometric_mean <- function(x) {
  prod(x) ^ (1 / length(x))
}

