test_that("returns dataframe by default", {
  out <- depreciation_interest(50000, 5000)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("Annuity factor", "Yearly depreciation and interest costs"))
  expect_true(is.numeric(out[[1]]) && is.numeric(out[[2]]))
})

test_that("annuity factor branch works", {
  a <- depreciation_interest(50000, 5000, output = "annuity_factor")
  expect_type(a, "double")
  expect_length(a, 1L)
})

test_that("annual cost branch works", {
  k <- depreciation_interest(50000, 5000, output = "annual_cost")
  expect_type(k, "double")
  expect_length(k, 1L)
})

test_that("zero interest uses correct limit", {
  a0 <- depreciation_interest(50000, 5000, n_amortisation_period = 8, i_interest_rt = 0, output = "annuity_factor")
  k0 <- depreciation_interest(50000, 5000, n_amortisation_period = 8, i_interest_rt = 0, output = "annual_cost")
  expect_equal(a0, 8)
  expect_equal(k0, (50000 - 5000) / 8)
})

test_that("input validation fires on bad args", {
  expect_error(depreciation_interest(-1, 0), "positive number")
  expect_error(depreciation_interest(1, -1), "non-negative")
  expect_error(depreciation_interest(1, 0, n_amortisation_period = 0), "positive number")
  expect_error(depreciation_interest(1, 0, i_interest_rt = -0.1), "non-negative")
})
