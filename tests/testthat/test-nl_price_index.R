# Failing test of issue 3
library(testthat)
library(tatooheene)

test_that("nl_price_index runs without error", {
  expect_silent(nl_price_index(start_year = 2022, end_year = 2023))
})

test_that("factor output compounds correctly and matches the table (regression test for issue #1)", {
  f   <- nl_price_index(start_year = 2013, end_year = 2023, output = "factor")
  tbl <- nl_price_index(start_year = 2013, end_year = 2023, output = "table")
  expect_equal(f, prod(tbl$Factor))
  expect_gt(length(f), 0)  # i.e. not the old numeric(0) bug
})

