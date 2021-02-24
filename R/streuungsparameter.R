#' @export
var_n <- function(x) {
  1 / n * sum((x - mean(x))^2)
}

#' @export
sd_n <- function(x) {
  sqrt(var_n(x))
}
