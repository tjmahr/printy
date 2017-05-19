
#' @export
fmt_minus_sign <- function(xs) {
  xs %>%
    stringr::str_replace("^-", "&minus;") %>%
    # Don't want a signed zero
    stringr::str_replace("^(&minus;)(0)$", "\\2") %>%
    stringr::str_replace("^(&minus;)(0[.]0+)$", "\\2")
}

# Don't print leading zero on bounded numbers.

#' @export
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
  stringr::str_replace(xs, "^(-?)0", "\\1")
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
    paste0("< ", .)

  xs_chr <- xs %>%
    fmt_fix_digits(digits) %>%
    fmt_leading_zero()

  xs_chr[xs < smallest_value] <- smallest_form
  xs_chr
}
