# Failing test of issue 3
library(testthat)
library(tatooheene)

test_that("nl_price_index runs without error", {
  expect_silent(nl_price_index(start_year = 2022, end_year = 2023))
})

