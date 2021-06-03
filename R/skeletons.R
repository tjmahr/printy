
#' Skeleton for a confidence interval
#'
#' `skel_conf_interval()` is a vectorized function. Use it to make multiple
#' intervals from, say, data-frame columns. `skel_conf_interval_pair()` is the
#' unvectorized function. Use it to make a single interval from a vector (pair) of two
#' numbers.
#'
#' @details These functions are wrappers around calls to `glue::glue()`.
#'
#' Originally, `skel_conf_interval()` was named `skel_conf_interval_v()`.
#'
#' @param xs a vector of the first elements in the intervals
#' @param ys a vector of the second elements in the intervals
#' @param x a vector of two elements to plug into the confidence interval
#' @param skeleton glue-style format to fill. defaults to `"[{xs}, {ys}]"` for
#'   `skel_conf_interval()` and `"[{x[1]}, {x[2]}]"` for
#'   `skel_conf_interval_pair()`.
#' @return strings representing confidence intervals
#' @name skel_conf_interval
#' @rdname skel_conf_interval
#' @examples
#' skel_conf_interval(c(.1, .2), c(.3, .4))
#' skel_conf_interval_pair(c(.1, .3))
NULL

#' @rdname skel_conf_interval
#' @export
skel_conf_interval <- function(xs, ys, skeleton = "[{xs}, {ys}]") {
  as.character(glue::glue(skeleton))
}

#' @rdname skel_conf_interval
#' @export
skel_conf_interval_pair <- function(x, skeleton = "[{x[1]}, {x[2]}]") {
  stopifnot(length(x) == 2)
  as.character(glue::glue(skeleton))
}

#' Skeleton for a range of numbers
#'
#' `skel_range()` is a vectorized function. Use it to make multiple range from,
#' say, data-frame columns. `skel_range_pair()` is the unvectorized function.
#' Use it to make a single range  from a vector (pair) of two numbers.
#'
#' @details These functions are wrappers around calls to `glue::glue()`.
#'
#' @param xs a vector of the first elements in the range
#' @param ys a vector of the second elements in the range
#' @param x a vector of two elements to plug into the range
#' @param skeleton glue-style format to fill. defaults to `"{xs}&ndash;{ys}"` for
#'   `skel_range()` and `"{x[1]}&ndash;{x[2]}"` for
#'   `skel_range_pair()`.
#' @return strings representing ranges
#' @name skel_range
#' @rdname skel_range
#' @examples
#' skel_range(c(.1, .2), c(.3, .4))
#' skel_range_pair(c(.1, .3))
NULL


#' Skeleton for missing values
#'
#' `skel_range()` is a vectorized function. Use it to make multiple range from,
#' say, data-frame columns. `skel_range_pair()` is the unvectorized function.
#' Use it to make a single range  from a vector (pair) of two numbers.
#'
#' @details These functions are wrappers around calls to `glue::glue()`.
#'
#' @param xs a vector of the first elements in the range
#' @param ys a vector of the second elements in the range
#' @param x a vector of two elements to plug into the range
#' @param skeleton glue-style format to fill. defaults to `"{xs}&ndash;{ys}"` for
#'   `skel_range()` and `"{x[1]}&ndash;{x[2]}"` for
#'   `skel_range_pair()`.
#' @return strings representing ranges
#' @name skel_range
#' @rdname skel_range
#' @examples
#' skel_range(c(.1, .2), c(.3, .4))
#' skel_range_pair(c(.1, .3))
NULL

#' @rdname skel_range
#' @export
skel_range_pair <- function(x, skeleton = "{x[1]}&ndash;{x[2]}") {
  stopifnot(length(x) == 2)
  as.character(glue::glue(skeleton))
}

#' @rdname skel_range
#' @export
skel_range <- function(xs, ys, skeleton = "{xs}&ndash;{ys}") {
  as.character(glue::glue(skeleton))
}


#' Skeletons for inline stats
#'
#' @param xs a vector of the values to plug into the skeleton
#' @param skeleton glue-style format to fill. defaults to `"SE&nbsp;= {x}"` for
#'   `skel_se()` and `"95% CI&nbsp;= {x}"` for `skel_ci()`.
#' @return strings with stats plugged in.
#' @export
#' @name skel_se
#' @rdname skel_se
skel_se <- function(x, skeleton = "SE&nbsp;= {x}") {
  as.character(glue::glue(skeleton))
}


#' @param ci_width width of the confidence interval to report. Defaults to
#'   `"95"`.
#' @rdname skel_se
#' @export
skel_ci <- function(
  x,
  ci_width = "95",
  skeleton = "{ci_width}% CI&nbsp;= {x}"
) {
  as.character(glue::glue(skeleton))
}


#' Skeleton for t-statistic-like functions
#'
#' This skeleton handles formats like t-statistics (`t(df) = value`) or
#' correlations (`r(df) = value`).
#'
#' @param x a two-element vector where the first number is the argument to the
#'   statistical function and the second is its value.
#' @param stat symbol for the statistic. defaults to `"t"`.
#' @param skeleton  glue-style format to fill. defaults to
#'   `"{stat}({x[1]})&nbsp;= {x[2]}"`.
#' @return the formatted string
#' @rdname skel_stat_n_value_pair
#' @export
skel_stat_n_value_pair <- function(
  x,
  stat = "t",
  skeleton = "{stat}({x[1]})&nbsp;= {x[2]}"
) {
  stopifnot(length(x) == 2)
  as.character(glue::glue(skeleton))
}
