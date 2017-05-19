context("strings")

test_that("str_tokenize() works", {
  expect_equal(str_tokenize("word"), c("w", "o", "r", "d"))
})
