#' Split a dataframe into a list of (lists of ...) dataframes
#'
#' This function is a streamlined, recursive version of
#' [`split()`][base::split()].
#'
#' @param .data a dataframe
#' @param ... (unquoted) names of columns to split by
#'
#' @return a list of dataframes when splitting by a single variable, a list of
#'   lists of dataframes when splitting by 2 variables, and so on.
#' @export
#'
#' @examples
#' # some kind of 2 by 2 design
#' df <- data.frame(
#'   x =    c(1, 2, 3, 4, 5, 6, 7, 8),
#'   time = c(1, 1, 2, 2, 1, 1, 2, 2),
#'   group = c("a", "a", "a", "a", "b", "b", "b", "b")
#' )
#'
#' super_split(df, group)
#'
#' super_split(df, time)
#'
#' # split by group and then split each of those by time
#' super_split(df, group, time)
super_split <- function(.data, ...) {
  dots <- rlang::enquos(...)
  for (var in seq_along(dots)) {
    var_name <- rlang::as_name(dots[[var]])
    .data <- purrr:::map_depth(
      .x = .data,
      .depth = var - 1,
      .f = function(xs) split(xs, xs[var_name])
    )
  }
  .data
}
