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

