test_that("super_split works with 1 variable", {
  df <- data.frame(
    x =    c(1, 2, 3, 4, 5, 6, 7, 8),
    time = c(1, 1, 2, 2, 1, 1, 2, 2),
    group = c("a", "a", "a", "a", "b", "b", "b", "b")
  )

  result_group <- list(
    # names are strings
    a = df[df$group == "a", ],
    b = df[df$group == "b", ]
  )

  expect_equal(super_split(df, group), result_group)

  result_time <- list(
    # names are numbers
    `1` = df[df$time == 1, ],
    `2` = df[df$time == 2, ]
  )

  expect_equal(super_split(df, time), result_time)
})

test_that("super_split works with 2 variables", {
  df <- data.frame(
    x =    c(1, 2, 3, 4, 5, 6, 7, 8),
    time = c(1, 1, 2, 2, 1, 1, 2, 2),
    group = c("a", "a", "a", "a", "b", "b", "b", "b")
  )

  result_group_time <- list()
  result_group <- list(
    # names are strings
    a = df[df$group == "a", ],
    b = df[df$group == "b", ]
  )

  result_group_time$a <- list(
    `1` = result_group$a[result_group$a$time == 1, ],
    `2` = result_group$a[result_group$a$time == 2, ]
  )

  result_group_time$b <- list(
    `1` = result_group$b[result_group$b$time == 1, ],
    `2` = result_group$b[result_group$b$time == 2, ]
  )

  expect_equal(super_split(df, group, time), result_group_time)

  result_time_group <- list()
  result_time <- list(
    # names are numbers
    `1` = df[df$time == 1, ],
    `2` = df[df$time == 2, ]
  )

  result_time_group$`1` <- list(
    a = result_time$`1`[result_time$`1`$group == "a", ],
    b = result_time$`1`[result_time$`1`$group == "b", ]
  )

  result_time_group$`2` <- list(
    a = result_time$`2`[result_time$`2`$group == "a", ],
    b = result_time$`2`[result_time$`2`$group == "b", ]
  )

  expect_equal(super_split(df, time, group), result_time_group)
})
