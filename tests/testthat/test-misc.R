
test_that("pretty random effects", {

  model <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  summary(model)

  results <- as.data.frame(tibble::tribble(
        ~Group,    ~Parameter, ~Variance,     ~SD, ~Correlations, ~`&nbsp;`,
     "Subject", "(Intercept)",  "612.10", "24.74",        "1.00", "&nbsp;",
      "&nbsp;",        "Days",   "35.07",  "5.92",         ".07",   "1.00",
    "Residual",      "&nbsp;",  "654.94", "25.59",      "&nbsp;", "&nbsp;"
  ))
  expect_equal(pretty_lme4_ranefs(model), results)
})
