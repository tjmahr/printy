#' Break a string to individual (character) tokens
#'
#' The usual job of this function is to break a string into a vector of
#' individual characters, but it can break strings using other separators.
#'
#' @param string a character vector of strings to break
#' @param pattern pattern to use for splitting. Defaults to `NULL` so that
#'   strings are split into individual characters.
#' @return a single character vector of the tokens
#' @export
#' @examples
#' str_tokenize(c("abc", "de"))
#' str_tokenize(c("abc de fg"), " ")
str_tokenize <- function(string, pattern = NULL) {
  unlist(strsplit(string, split = pattern, perl = TRUE))
}

#' Replace strings that duplicate the previous string
#'
#' The common use of this function to clean up columns in a presentation-quality
#' table.
#' @param string a character vector
#' @param replacement text to use as a replacement for duplicated values
#' @return a single character vector with immediately repeating items replaced
#' @export
#' @examples
#' str_replace_same_as_previous(
#'   c("a", "a", "a", "b" , "b", "c", "d", "d"),
#'   ""
#' )
str_replace_same_as_previous <- function(string, replacement) {
  string[is_same_as_previous(string)] <- replacement
  string
}

# Is x[n] the same as x[n-1]
is_same_as_previous <- function(xs) {
  same_as_previous <- xs == dplyr::lag(xs)

  if (length(xs) > 0) {
    # Overwrite NA (first lag) from lag(xs)
    same_as_previous[1] <- FALSE
  }

  same_as_previous
}
