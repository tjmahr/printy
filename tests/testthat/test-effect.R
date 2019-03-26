context("reporting effects from models")

# library(testthat)

model <- stats::lm(breaks ~ wool, datasets::warpbreaks)
# saveRDS(model, file = testthat::test_path("data/lm-model.rds"))


test_that("fmt_effect_md() matches hand-formatted results on lm() models", {
  # model <- readRDS(testthat::test_path("data/lm-model.rds"))
  model_summary <- stats::coef(summary(model))

  b_value <- model_summary["woolB", "Estimate", drop = TRUE] %>%
    fmt_fix_digits(2) %>%
    fmt_minus_sign()

  # This also tests `digits = 3`
  e_value <- model_summary["woolB", "Std. Error", drop = TRUE] %>%
    fmt_fix_digits(3)

  s_value <- model_summary["woolB", "t value", drop = TRUE] %>%
    fmt_fix_digits(2) %>%
    fmt_minus_sign()

  p_value <- model_summary["woolB", "Pr(>|t|)", drop = TRUE] %>%
    fmt_fix_digits(2) %>%
    fmt_leading_zero()

  b_manual <- paste0("*b*&nbsp;= ", b_value)
  b_printy <- fmt_effect_md(model, "woolB", "b")

  e_manual <- paste0("SE&nbsp;= ", e_value)
  e_printy <- fmt_effect_md(model, "woolB", "e", digits = 3)

  s_manual <- paste0("*t*&nbsp;= ", s_value)
  s_printy <- fmt_effect_md(model, "woolB", "s")

  S_manual <- paste0("*t*(", model$df.residual, ")&nbsp;= ", s_value)
  S_printy <- fmt_effect_md(model, "woolB", "S")

  p_manual <- paste0("*p*&nbsp;= ", p_value)
  p_printy <- fmt_effect_md(model, "woolB", "p")


  i_manual <- stats::confint(model)["woolB", ] %>%
    as.vector() %>%
    fmt_fix_digits(2) %>%
    fmt_minus_sign() %>%
    skel_conf_interval() %>%
    paste0("95% CI&nbsp;= ", .)

  i_printy <- fmt_effect_md(model, "woolB", "i")

  expect_equal(b_manual, b_printy)
  expect_equal(e_manual, e_printy)
  expect_equal(s_manual, s_printy)
  expect_equal(S_manual, S_printy)
  expect_equal(p_manual, p_printy)
  expect_equal(i_manual, i_printy)


  # # Convincing myself that the df.residual is the one used in the t-value
  # 2 * pt(abs(12.409761), model$df.residual, lower.tail = FALSE)
  # 2 * pt(abs(-1.633537), model$df.residual, lower.tail = FALSE)



})
