context("formatting")

test_that("fmt_minus_sign() handles regular numbers", {
  test <- c(0, 1, 2L, 1.00009, -1, -2, -0.5, -0.006, NA)
  want <- c("0", "1", "2", "1.00009", "&minus;1", "&minus;2",
            "&minus;0.5", "&minus;0.006", NA)
  expect_equal(fmt_minus_sign(test), want)
})

test_that("fmt_minus_sign() removes sign from negative zero", {
  test <- c(-0, -0L, -0.00)
  want <- c("0", "0", "0")
  expect_equal(fmt_minus_sign(test), want)

  test <- c("-0", "-0.00")
  want <- c("0", "0.00")
  expect_equal(fmt_minus_sign(test), want)
})

test_that("fmt_replace_na() replaces NA values", {
  expect_equal(fmt_replace_na(NA, "<missing>"), "<missing>")

  # Defaults to empty strings
  test <- c(-1:3, NA)
  want <- c("-1", "0", "1", "2", "3", "")
  expect_equal(fmt_replace_na(test), want)
})

test_that("fmt_replace_na() does not replace \"NA\"", {
  expect_equal(fmt_replace_na(c("hey", "NA")), c("hey", "NA"))
})


test_that("fmt_p_value() prints small values with less-thans, like \"< .001\"", {
  ps <- c(1.42950220581308e-12, 4.86751586760195e-08, 1.07359248017686e-23,
          0.0388882596082964, 0.00305963409612887, 0.00258434378890403, .6)

  ps_1 <- c("< .1",    "< .1",    "< .1",    "< .1",  "< .1",  "< .1",  ".6")
  ps_2 <- c("< .01",   "< .01",   "< .01",   ".04",   "< .01", "< .01", ".60")
  ps_3 <- c("< .001",  "< .001",  "< .001",  ".039",  ".003",  ".003",  ".600")
  ps_4 <- c("< .0001", "< .0001", "< .0001", ".0389", ".0031", ".0026", ".6000")

  expect_equal(fmt_p_value(ps, 1), ps_1)
  expect_equal(fmt_p_value(ps, 2), ps_2)
  expect_equal(fmt_p_value(ps, 3), ps_3)
  expect_equal(fmt_p_value(ps, 4), ps_4)
})

