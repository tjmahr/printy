#' Format negative numbers with a minus sign
#'
#' @param xs a vector of numbers or a character vector representing numbers
#' @return the vector with leading hyphens replaced with HTML minus signs
#'   (`&minus;`).
#' @export
#' @details Negative zero `-0`, which might happen from aggressive rounding,
#'   does not get a minus sign.
#' @examples
#' fmt_minus_sign(c(1, .2, -1, -.2))
#'
#' # Don't allow zero to be signed
#' fmt_minus_sign(c(-0, round(-0.001)))
fmt_minus_sign <- function(xs) {
  xs %>%
    stringr::str_replace("^-", "&minus;") %>%
    # Don't want a signed zero
    stringr::str_replace("^(&minus;)(0)$", "\\2") %>%
    stringr::str_replace("^(&minus;)(0[.]0+)$", "\\2")
}

#' Format numbers to remove leading zeros
#'
#' @param xs a vector of numbers or a character vector representing numbers
#' @return the vector with leading zeros removed. This function returns a
#'   warning if any of the values have an absolute value greater than 1.
#' @export
#' @details APA format says that values that are bounded between \[-1, 1\]
#'   should not be formatted with a leading zero. Common examples would be
#'   correlations, proportions, probabilities and p-values. Why print the digit
#'   if it's almost never used?
#'
#'   Zeros are printed to match the precision of the most precise number. For
#'   example, `c(0, 0.111)` becomes `c(.000, .111)`
#' @examples
#' fmt_leading_zero(c(0, 0.111))
#' fmt_leading_zero(c(0.99, -0.9, -0.0))
fmt_leading_zero <- function(xs) {
  digit_matters <- xs %>%
    as.numeric() %>%
    abs() %>%
    # Problem if any value is greater than 1.0
    is_greater_than_1() %>%
    stats::na.omit()

  if (any(digit_matters)) {
    warning("Non-zero leading digit")
  }

  replaced <- stringr::str_replace(xs, "^(-?)0", "\\1")

  if (any(as.numeric(xs) == 0, na.rm = TRUE)) {
    # Match the most precise number (or use .0)
    precision <- max(c(stringr::str_count(replaced, "\\d"), 1))
    new_zero <- paste0(".", paste0(rep(0, precision), collapse = ""))
    replaced[xs == 0] <- new_zero
  }

  replaced
}

is_greater_than_1 <- function(xs) {
  xs > 1
}

#' @export
fmt_fix_digits <- function(xs, n = 2) {
  stopifnot(length(n) == 1)
  rounded_xs <- round(xs, n)
  decimals <- if (n < 0) 0 else n
  printed <- sprintf("%.*f", decimals, rounded_xs)
  printed[is.na(xs)] <- NA
  printed
}

#' @export
fmt_replace_na <- function(xs, replacement = "") {
  ifelse(is.na(xs), replacement, xs)
}


#' @export
fmt_p_value <- function(xs, digits = 3) {
  stopifnot(digits >= 1, length(digits) == 1)

  smallest_value <- 1 / (10 ^ digits)
  smallest_form <-  smallest_value %>%
    fmt_fix_digits(digits) %>%
    fmt_leading_zero() %>%
    paste0_after(.first = "< ")

  xs_chr <- xs %>%
    fmt_fix_digits(digits) %>%
    fmt_leading_zero()

  xs_chr[xs < smallest_value] <- smallest_form
  xs_chr
}

paste0_after <- function(..., .first) {
  paste0(.first, ...)
}

#' Format a *p*-value in markdown
#'
#' Values less than .06 are formatted with 3 digits. Values equal to .06 or
#' greater are formatted with 2 digits.
#'
#' [scales::label_pvalue()] does the initial rounding and formatting. Then this
#' function strips off the leading 0 of the *p* value.
#'
#' @param ps *p*-values to format
#' @return a character vector of markdown formatted *p*-values
#'
#' @export
#' @examples
#' fmt_p_value_md(0.0912)
#' fmt_p_value_md(0.0512)
#' fmt_p_value_md(0.005)
#'
#' # "p less than" notation kicks in below .001.
#' fmt_p_value_md(0.0005)
fmt_p_value_md <- function(ps) {
  prefixes <- c("*p*&nbsp;< ", "*p*&nbsp;= ", "*p*&nbsp;> ")
  label_pvalue_2 <- scales::label_pvalue(accuracy = .01 , prefix = prefixes)
  label_pvalue_3 <- scales::label_pvalue(accuracy = .001, prefix = prefixes)

  # use three digits if less than .06
  ps <- ifelse(
    ps < .06 | is.na(ps),
    label_pvalue_3(ps),
    label_pvalue_2(ps)
  )

  ps %>%
    stringr::str_replace("(=|<|>) 0[.]", "\\1 .")
}
