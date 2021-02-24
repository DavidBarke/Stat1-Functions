#' Summarise sorted data
#'
#' @param x A numeric vector.
#'
#' @export
sorted_data <- function(x) {
  tbl <- dplyr::tibble(a_j = x) %>%
    dplyr::group_by(a_j) %>%
    dplyr::summarise(h_j = dplyr::n()) %>%
    dplyr::mutate(
      f_j = h_j / sum(h_j),
      H_j = cumsum(h_j),
      F_j = cumsum(f_j)
    )

  structure(
    tbl,
    class = c("sorted_data", class(tbl))
  )
}

#' @export
mean.sorted_data <- function(x, ...) {
  sum(x$a_j * x$f_j)
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

#' Summarise grouped data
#'
#' @param breaks A numeric vector with length *n + 1* where *n* is the number of
#'   groups. Group *i* is the right-open interval [`breaks[i]`, `breaks[i + 1]`).
#'   Group *n* is right-closed.
#' @param x A numeric vector. See section `x_type`.
#' @param x_type See section `x_type`.
#' @param n Sample size (only used if `x_type = "f_j"`)
#'
#' @section x_type:
#'
#' Based on `x_type` the argument `x` is treated as follows:
#'
#' * `x` : Values to be grouped.
#' * `h_j` : Absolute frequencies of each group. Then
#'   `length(x) == length(breaks) - 1`.
#' * `f_j` : Relative frequencies of each group. Then
#'   `length(x) == length(breaks) - 1`. In this case the sample size `n` has
#'   to be specified.
#'
#' @export
grouped_data <- function(breaks, x, x_type = c("x", "h_j", "f_j"), n) {
  x_type <- match.arg(x_type)

  if (is.unsorted(breaks)) stop("'breaks' must be sorted!")

  from <- breaks[1:(length(breaks) - 1)]
  to <- breaks[2:length(breaks)]

  tbl <- if (x_type == "x") {
    stopifnot(all(x >= breaks[1] & x <= breaks[length(breaks)]))

    groups <- findInterval(
      x, breaks, rightmost.closed = TRUE
    )

    dplyr::tibble(
      from = from,
      to = to,
      h_j = table(groups)
    )
  } else if (x_type == "h_j") {
    stopifnot(length(x) == length(breaks) - 1)

    dplyr::tibble(
      from = from,
      to = to,
      h_j = x
    )
  } else {
    stopifnot(length(x) == length(breaks) - 1)
    if (sum(x) != 1) stop("Relative frequencies must add up to 1!")

    dplyr::tibble(
      from = from,
      to = to,
      h_j = x * n
    )
  }

  tbl <- tbl %>%
    dplyr::mutate(
      f_j = h_j / sum(h_j),
      b_j = to - from,
      m_j = (from + to) / 2,
      H_j = cumsum(h_j),
      F_j = cumsum(f_j)
    )

  structure(
    tbl,
    class = c("grouped_data", class(tbl))
  )
}

#' @export
mean.grouped_data <- function(x, ...) {
  sum(x$m_j * x$f_j)
}
