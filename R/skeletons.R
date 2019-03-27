
#' Skeleton for a confidence interval
#'
#' `skel_conf_interval_v()` is the vectorized function. Use it to make multiple
#' intervals from, say, data-frame columns. `skel_conf_interval()` is the
#' unvectorized function. Use it to make a single interval from a vector of two
#' numbers.
#'
#' @details These functions are wrappers around calls to `glue::glue()`.
#' @param xs a vector of the first elements in the intervals
#' @param ys a vector of the second elements in the intervals
#' @param x a vector of two elements to plug into the confidence interval
#' @param skeleton glue-style format to fill. defaults to `"[{xs}, {ys}]"` for
#'   `skel_conf_interval_v()` and `"[{x[1]}, {x[2]}]"` for
#'   `skel_conf_interval()`.
#' @return strings representing confidence intervals
#' @name skel_conf_interval
#' @rdname skel_conf_interval
#' @examples
#' skel_conf_interval_v(c(.1, .2), c(.3, .4))
#' skel_conf_interval(c(.1, .3))
NULL

#' @rdname skel_conf_interval
#' @export
skel_conf_interval_v <- function(xs, ys, skeleton = "[{xs}, {ys}]") {
  as.character(glue::glue(skeleton))
}

#' @rdname skel_conf_interval
#' @export
skel_conf_interval <- function(x, skeleton = "[{x[1]}, {x[2]}]") {
  stopifnot(length(x) == 2)
  as.character(glue::glue(skeleton))
}


#' Skeleton for t-statistic-like functions
#'
#' This skeleton handles formats like t-statistics (`t(df) = value`) or
#' correlations (`r(n) = value`).
#'
#' @param x a two-element vector where the first number is the argument to the
#'   statistical function and the second is its value.
#' @param stat symbol for the statistic. defaults to `"t"`.
#' @param skeleton  glue-style format to fill. defaults to
#'   `"{stat}({x[1]})&nbsp;= {x[2]}"`.
#' @return the formatted string
#' @rdname skel_stat_n_value
#' @export
skel_stat_n_value <- function(x, stat = "t",
                              skeleton = "{stat}({x[1]})&nbsp;= {x[2]}") {
  stopifnot(length(x) == 2)
  as.character(glue::glue(skeleton))
}


