#' @export
str_tokenize <- function(string) {
  string %>%
    strsplit(NULL) %>%
    unlist()
}

#' @export
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
