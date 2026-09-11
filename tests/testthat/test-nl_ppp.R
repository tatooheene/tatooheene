test_that("single year works", {
  val <- nl_ppp(year = 2019)
  expect_type(val, "double")
  expect_length(val, 1L)
})

test_that("multiple years work (regression test for #2)", {
  vals <- nl_ppp(year = c(2019, 2020))
  expect_type(vals, "double")
  expect_length(vals, 2L)
})

test_that("default returns the full dataset", {
  out <- nl_ppp()
  expect_true(is.data.frame(out))
  expect_true(all(c("Year", "PPP") %in% names(out)))
})
