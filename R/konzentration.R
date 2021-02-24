#' Lorenz concentration curve
#'
#' Table containing data constituting the Lorenz concentration curve.
#'
#' @param x A numeric vector.
#'
#' @export
lorenz_concentration <- function(x) {
  dplyr::tibble(a_j = x) %>%
    dplyr::group_by(a_j) %>%
    dplyr::summarise(h_j = dplyr::n()) %>%
    dplyr::mutate(
      f_j = h_j / sum(h_j),
      u_j = cumsum(f_j),
      `a_j * h_j / V` = a_j * h_j / sum(a_j * h_j),
      v_j = cumsum(`a_j * h_j / V`)
    )
}

#' Gini coefficient
#'
#' @param x A numeric vector.
#'
#' @export
gini_coefficient <- function(x) {
  lorenz <- lorenz_concentration(x)
  area_below_lorenz <- pracma::trapz(c(0, lorenz$u_j), c(0, lorenz$v_j))
  1 - 2 * area_below_lorenz
}

#' Modified Gini coefficient
#'
#' @rdname gini_coefficient
#'
#' @export
modified_gini_coefficient <- function(x) {
  g <- gini_coefficient(x)
  n <- length(x)
  n / (n - 1) * g
}

#' Herfindahl index
#'
#' @param x A numeric vector.
#'
#' @export
herfindahl_index <- function(x) {
  sum((x / sum(x))^2)
}

#' Modified Herfindahl index
#'
#' @rdname herfindahl_index
#'
#' @export
modified_herfindahl_index <- function(x) {
  herfindahl_index(x) / length(x)
}
