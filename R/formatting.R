
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

#' @export
fmt_p_value_md <- function(ps) {
  # use three digits if less than .6
  ps <- ifelse(
    ps < .06 | is.na(ps),
    scales::pvalue(ps, accuracy = .001, add_p = TRUE),
    scales::pvalue(ps, accuracy = .01, add_p = TRUE))

  ps %>%
    stringr::str_replace("(=|<)0[.]", "\\1.") %>%
    stringr::str_replace("p(<|=)", "*p*&nbsp;\\1 ")
}


#' @export
fmt_effect_md <- function(model, effect, terms = "besp", digits = 2,
                          statistic = "t", b_lab = NULL, ci_width = .95) {

  stopifnot(length(digits) %in% c(1, nchar(terms)))
  stopifnot(inherits(model, "lm"))

  if (length(digits) == 1) {
    digits <- rep(digits, nchar(terms))
  }

  to_get <- unlist(stringr::str_split(terms, ""))
  output <- to_get

  b_lab <- ifelse(is.null(b_lab), effect, b_lab)

  for (item_i in seq_along(to_get)) {
    item <- to_get[item_i]

    output[item_i] <- switch(
      item,

      B = mod_get_b(model, effect) %>%
        fmt_fix_digits(digits[item_i]) %>%
        fmt_minus_sign() %>%
        prefix_equals("*b*", b_lab),

      b = mod_get_b(model, effect) %>%
        fmt_fix_digits(digits[item_i]) %>%
        fmt_minus_sign() %>%
        prefix_equals("*b*"),

      e = mod_get_e(model, effect) %>%
        fmt_fix_digits(digits[item_i]) %>%
        prefix_equals("SE"),

      i = mod_get_i(model, effect, ci_width = ci_width) %>%
        fmt_fix_digits(digits[item_i]) %>%
        fmt_minus_sign() %>%
        skel_conf_interval() %>%
        prefix_equals(
          paste0(scales::percent(ci_width, accuracy = 1), " CI")
        ),

      s = mod_get_s(model, effect) %>%
        fmt_fix_digits(digits[item_i]) %>%
        fmt_minus_sign() %>%
        prefix_equals(md_ital(statistic)),

      S = mod_get_s(model, effect) %>%
        fmt_fix_digits(digits[item_i]) %>%
        fmt_minus_sign() %>%
        prefix_equals(
          paste0(md_ital(statistic), "(", mod_get_residual_df(model), ")")),

      p = mod_get_p(model, effect) %>%
        fmt_p_value_md(),

      NA
      )
  }

  paste0(output, collapse = ", ")

}


prefix_equals <- function(x, main, sub = NULL) {
  if (is.null(sub)) {
    paste0(main, "&nbsp;= ", x)
  } else {
    paste0(main, "<sub>", sub, "</sub>", "&nbsp;= ", x)
  }
}

md_ital <- function(x) paste0("*", x, "*")

mod_get_b <- function(model, effect) {
  summary <- broom::tidy(model)
  stopifnot(effect %in% summary$term)
  unlist(summary[summary$term == effect, "estimate"], use.names = FALSE)
}

mod_get_p <- function(model, effect) {
  summary <- broom::tidy(model)
  stopifnot(effect %in% summary$term)
  unlist(summary[summary$term == effect, "p.value"], use.names = FALSE)
}

mod_get_e <- function(model, effect) {
  summary <- broom::tidy(model)
  stopifnot(effect %in% summary$term)
  unlist(summary[summary$term == effect, "std.error"], use.names = FALSE)
}

mod_get_s <- function(model, effect) {
  summary <- broom::tidy(model)
  stopifnot(effect %in% summary$term)
  unlist(summary[summary$term == effect, "statistic"], use.names = FALSE)
}

mod_get_residual_df <- function(model) {
  summary <- broom::glance(model)
  stopifnot("df.residual" %in% names(summary))
  summary[["df.residual"]]
}

mod_get_i <- function(model, effect, ci_width) {
  summary <- broom::tidy(model, conf.int = TRUE, conf.level = ci_width)
  stopifnot(effect %in% summary$term)
  unlist(
    summary[summary$term == effect, c("conf.low", "conf.high")],
    use.names = FALSE
  )
}


