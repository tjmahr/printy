context("strings")

test_that("str_tokenize() works", {
  expect_equal(str_tokenize("word"), c("w", "o", "r", "d"))
})

test_that("str_replace_same_as_previous() works", {
  expect_equal(
    str_replace_same_as_previous(c("a", "a", "a", "b", "b", "c", "a"), "-"),
    c("a", "-", "-", "b", "-", "c", "a"))
})
