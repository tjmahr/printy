
#' Format an effect from a model object in markdown
#'
#' @param model a model object
#' @param effect string naming an effect from a model
#' @param terms a string representing the terms about the effect to extract and
#'   format and the order to print the terms. See details below. Defaults to
#'   `"besp"` for parameter estimate, standard error, statistic, *p*-value.
#' @param digits a vector of digits to use for non-*p*-value terms. Defaults to
#'   2 for 2 decimal places of precision for all terms. This argument can be a
#'   vector to set the digits for each term, but in this case, the digits is
#'   still ignored for *p*-values.
#' @param statistic symbol to use for statistic. Defaults to *t*.
#' @param b_lab label to print in subscripts after *b* for when `"B"` is one of
#'   the terms.
#' @param ci_width width to use for confidence intervals when the term `"i"` is
#'   used.
#' @export
#'
#' @details Currently only effects fit by [stats::lm()] and [lme4::lmer()].
#'
#' The supported terms are:
#'
#' * `"b"` - parameter estimate (think b for _beta_)
#' * `"B"` - parameter estimate with a subscript label provided by `b_lab`
#' * `"e"` - standard error
#' * `"s"` - statistic. The symbol for the statistic is set by
#'   `statistic`. The default value is `"t"` for a *t*-statistic. Example
#'   output: _t_ = 1.
#' * `"S"` - statistic as in `"s"` but with degrees of freedom. Example
#'   output: _t_(12) = 1.
#' * `"i"` - confidence interval. Width is set by `ci_width`.
#' * `"p"` - _p_-value. The p-value is formatted by [fmt_p_value_md()].
#'
#' Degrees of freedom and *p*-values for `lmer()` models use the
#' Kenwood-Rogers approximation provided by [parameters::p_value_kenward()].
#' This computation can take a while. The confidence-interval calculation uses
#' default confidence interval calculation method used by
#' [`broom.mixed::tidy.merMod()`][broom::lme4_tidiers].
#'
#' @examples
#' model <- lm(breaks ~ wool * tension, warpbreaks)
#'
#' # default to: b (beta), e (error), s (statistic), p (p value)
#' fmt_effect_md(model, "woolB", "besp")
#'
#' fmt_effect_md(model, "woolB", "Besp", b_lab = "WoolB")
#'
#' fmt_effect_md(model, "woolB", "i")
fmt_effect_md <- function(
  model,
  effect,
  terms = "besp",
  digits = 2,
  statistic = "t",
  b_lab = NULL,
  ci_width = .95
) {
  stopifnot(length(digits) %in% c(1, nchar(terms)))
  stopifnot(inherits(model, c("lm", "lmerMod")))

  if (length(digits) == 1) {
    digits <- rep(digits, nchar(terms))
  }

  term_values <- get_terms(model, effect, terms, ci_width = ci_width)
  output <- seq_along(term_values)

  b_lab <- ifelse(is.null(b_lab), effect, b_lab)

  for (item_i in seq_along(term_values)) {
    item_value <- term_values[[item_i]]
    item_name <- names(term_values[item_i])

    output[item_i] <- switch(
      item_name,

      B = item_value %>%
        fmt_fix_digits(digits[item_i]) %>%
        fmt_minus_sign() %>%
        prefix_equals("*b*", b_lab),

      b = item_value %>%
        fmt_fix_digits(digits[item_i]) %>%
        fmt_minus_sign() %>%
        prefix_equals("*b*"),

      e = item_value %>%
        fmt_fix_digits(digits[item_i]) %>%
        prefix_equals("SE"),

      i = item_value %>%
        fmt_fix_digits(digits[item_i]) %>%
        fmt_minus_sign() %>%
        skel_conf_interval() %>%
        prefix_equals(
          paste0(scales::percent(ci_width, accuracy = 1), " CI")
        ),

      s = item_value %>%
        fmt_fix_digits(digits[item_i]) %>%
        fmt_minus_sign() %>%
        prefix_equals(md_ital(statistic)),

      S = item_value %>%
        round_S(digits[item_i]) %>%
        fmt_minus_sign() %>%
        skel_stat_n_value(stat = md_ital(statistic)),

      p = item_value %>%
        fmt_p_value_md(),

      NA
    )
  }

  paste0(output, collapse = ", ")
}


# round the first item (degrees of freedom), fix digits on second (statistic)
round_S <- function(x, digits) {
  c(round(x[1], digits), fmt_fix_digits(x[2], digits))
}

prefix_equals <- function(x, main, sub = NULL) {
  if (is.null(sub)) {
    paste0(main, "&nbsp;= ", x)
  } else {
    paste0(main, "<sub>", sub, "</sub>", "&nbsp;= ", x)
  }
}

md_ital <- function(x) paste0("*", x, "*")


get_terms <- function(model, effect, terms, ...) {
  UseMethod("get_terms")
}

get_terms.default <- function(model, effect, terms, ci_width = .95) {
  to_get <- str_tokenize(terms)
  ci <- "i" %in% to_get

  summary <- broom::tidy(
    model,
    conf.int = ci,
    conf.level = ci_width
  )

  if (! effect %in% summary[["term"]]) {
    stop(rlang::as_label(effect), " is not a parameter name")
  }

  if ("S" %in% to_get) {
    summary[["df"]] <- mod_get_residual_df(model)
  }

  summary <- summary[summary$term == effect, ]

  to_get %>%
    lapply(function(t) get_term_from_broom(t, summary)) %>%
    stats::setNames(to_get)
}

get_terms.lmerMod <- function(model, effect, terms, ci_width = .95) {
  to_get <- str_tokenize(terms)
  ci <- "i" %in% to_get

  summary <- broom.mixed::tidy(
    model,
    effects = "fixed",
    conf.int = ci,
    conf.level = ci_width
  )

  if (! effect %in% summary[["term"]]) {
    stop(rlang::as_label(effect), " is not a parameter name")
  }

  # Use Kenwood Rogers approximation
  use_kr <- any(c("S", "p") %in% to_get)
  if (use_kr) {

    kr_test <- parameters::p_value_kenward(model) %>%
      as.data.frame() %>%
      dplyr::rename(term = .data$Parameter, p.value = .data$p)

    kr_test[["df"]] <- parameters::dof_kenward(model)
    kr_test[["std.error"]] <- parameters::se_kenward(model)[["SE"]]

    kr_test[["statistic"]] <- lme4::fixef(model) / kr_test[["std.error"]]

    summary[["std.error"]] <- NULL
    summary[["statistic"]] <- NULL
    summary <- dplyr::left_join(summary, kr_test, by = "term")
  }

  summary <- summary[summary$term == effect, ]

  to_get %>%
    lapply(function(t) get_term_from_broom(t, summary)) %>%
    stats::setNames(to_get)
}


get_term_from_broom <- function(term, summary) {
  slist <- as.list(summary)

  switch(
    term,
    b = slist[["estimate"]],
    B = slist[["estimate"]],
    e = slist[["std.error"]],
    s = slist[["statistic"]],
    S = c(slist[["df"]], slist[["statistic"]]),
    p = slist[["p.value"]],
    i = c(slist[["conf.low"]], slist[["conf.high"]]),
    NA
  )
}


mod_get_residual_df <- function(model, ...) UseMethod("mod_get_residual_df")

mod_get_residual_df.default <- function(model) {
  summary <- broom::glance(model)
  stopifnot("df.residual" %in% names(summary))
  summary[["df.residual"]]
}
