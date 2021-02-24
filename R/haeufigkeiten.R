#' Summarise sorted data
#'
#' @param x A numeric vector.
#'
#' @export
sorted_data <- function(x) {
  dplyr::tibble(a_j = x) %>%
    dplyr::group_by(a_j) %>%
    dplyr::summarise(h_j = dplyr::n()) %>%
    dplyr::mutate(
      f_j = h_j / sum(h_j),
      H = cumsum(h_j),
      `F` = cumsum(f_j)
    )
}

#' Transform contingency table of counts to vector
#'
#' @param x A numeric vector with unique values.
#' @param count A numeric vector with same length as `x`.
#'
#' @export
sorted_to_vector <- function(x, count) {
  stopifnot(length(x) == length(count))
  rep(x, times = count)
}
