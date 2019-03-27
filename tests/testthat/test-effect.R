context("reporting effects from models")

test_that("fmt_effect_md() matches hand-formatted results on lm() models", {
  # model <- readRDS(testthat::test_path("data/lm-model.rds"))
  model <- stats::lm(breaks ~ wool, datasets::warpbreaks)
  model_summary <- stats::coef(summary(model))
  effect <- "woolB"

  b_value <- model_summary[effect, "Estimate", drop = TRUE] %>%
    fmt_fix_digits(2) %>%
    fmt_minus_sign()

  # This also tests `digits = 3`
  e_value <- model_summary[effect, "Std. Error", drop = TRUE] %>%
    fmt_fix_digits(3)

  s_value <- model_summary[effect, "t value", drop = TRUE] %>%
    fmt_fix_digits(2) %>%
    fmt_minus_sign()

  p_value <- model_summary[effect, "Pr(>|t|)", drop = TRUE] %>%
    fmt_fix_digits(2) %>%
    fmt_leading_zero()

  b_manual <- paste0("*b*&nbsp;= ", b_value)
  b_printy <- fmt_effect_md(model, effect, "b")

  e_manual <- paste0("SE&nbsp;= ", e_value)
  e_printy <- fmt_effect_md(model, effect, "e", digits = 3)

  s_manual <- paste0("*t*&nbsp;= ", s_value)
  s_printy <- fmt_effect_md(model, effect, "s")

  S_manual <- paste0("*t*(", model$df.residual, ")&nbsp;= ", s_value)
  S_printy <- fmt_effect_md(model, effect, "S")

  p_manual <- paste0("*p*&nbsp;= ", p_value)
  p_printy <- fmt_effect_md(model, effect, "p")

  i_manual <- stats::confint(model)[effect, ] %>%
    as.vector() %>%
    fmt_fix_digits(2) %>%
    fmt_minus_sign() %>%
    skel_conf_interval() %>%
    paste0("95% CI&nbsp;= ", .)

  i_printy <- fmt_effect_md(model, effect, "i")

  expect_equal(b_manual, b_printy)
  expect_equal(e_manual, e_printy)
  expect_equal(s_manual, s_printy)
  expect_equal(S_manual, S_printy)
  expect_equal(p_manual, p_printy)
  expect_equal(i_manual, i_printy)
})


test_that("fmt_effect_md() fails on missing parameters", {
  model <- stats::lm(breaks ~ wool, datasets::warpbreaks)
  expect_error(fmt_effect_md(model, "intercept"), "not a parameter name")
})


test_that("fmt_effect_md() handles lmer() models", {
  skip_on_cran()

  if (!requireNamespace("pbkrtest", quietly = TRUE)) {
    skip("pbkrtest is not available")
  }

  data(beets, package = "pbkrtest")

  f1 <- sugpct ~ block + sow + harvest + (1 | block:harvest)
  f2 <- sugpct ~ block + sow +         + (1 | block:harvest)
  model  <- lme4::lmer(f1, beets)
  model2 <- lme4::lmer(f2, beets)
  model_summary <- stats::coef(summary(model))
  effect <- "harvestharv2"

  b_value <- model_summary[effect, "Estimate", drop = TRUE] %>%
    fmt_fix_digits(2) %>%
    fmt_minus_sign()

  e_value <- model_summary[effect, "Std. Error", drop = TRUE] %>%
    fmt_fix_digits(3)

  s_value <- model_summary[effect, "t value", drop = TRUE] %>%
    fmt_fix_digits(2) %>%
    fmt_minus_sign()

  b_manual <- paste0("*b*&nbsp;= ", b_value)
  b_printy <- fmt_effect_md(model, effect, "b")

  e_manual <- paste0("SE&nbsp;= ", e_value)
  e_printy <- fmt_effect_md(model, effect, "e", digits = 3)

  s_manual <- paste0("*t*&nbsp;= ", s_value)
  s_printy <- fmt_effect_md(model, effect, "s")

  i_manual <- stats::confint(model, method = "Wald")[effect, ] %>%
    as.vector() %>%
    fmt_fix_digits(2) %>%
    fmt_minus_sign() %>%
    skel_conf_interval() %>%
    paste0("95% CI&nbsp;= ", .)

  i_printy <- fmt_effect_md(model, effect, "i")

  expect_equal(b_manual, b_printy)
  expect_equal(e_manual, e_printy)
  expect_equal(s_manual, s_printy)
  expect_equal(i_manual, i_printy)

  # Get p-value and degrees of freedom from Kenwood-Rogers
  kr <- pbkrtest::KRmodcomp(model, model2)
  df <- kr$stats$ddf %>% round(2) %>% as.character()

  S_manual <- skel_stat_n_value(c(df, s_value), stat = "*t*")
  S_printy <- fmt_effect_md(model, effect, "S")

  p_manual <- kr$stats$p.value %>%
    fmt_p_value_md()
  p_printy <- fmt_effect_md(model, effect, "p")

  expect_equal(S_manual, S_printy)
  expect_equal(p_manual, p_printy)
})
