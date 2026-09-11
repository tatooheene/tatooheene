test_that("matrix times is accepted and matches vector times (regression test for #3)", {
  m <- matrix(c(0, 1, 2), nrow = 3)
  out_matrix <- apply_discounting(values = rep(100, 3), discount_rate = "costs", times = m)
  out_vector <- apply_discounting(values = rep(100, 3), discount_rate = "costs", times = c(0, 1, 2))
  expect_equal(out_matrix, out_vector)
})

test_that("aggregate sums the discounted values", {
  total <- apply_discounting(values = rep(100, 3), discount_rate = "costs",
                             times = c(0, 1, 2), aggregate = TRUE)
  expect_length(total, 1L)
})

test_that("non-standard discount rate messages instead of erroring", {
  expect_message(
    apply_discounting(values = 100, discount_rate = 0.04, times = 1),
    "different than the one recommended"
  )
})
