test_that("default formatting uses 2 digits and EUR", {
  expect_equal(pretty_price(1000), "1,000.00 EUR")
})

test_that("custom currency is appended", {
  expect_equal(pretty_price(5, currency = "USD", digits = 0), "5 USD")
})

test_that("custom digits are respected", {
  expect_equal(pretty_price(1234.5, digits = 2), "1,234.50 EUR")
  expect_equal(pretty_price(1234567.891, digits = 3), "1,234,567.891 EUR")
})

test_that("big numbers get a thousands separator", {
  expect_equal(pretty_price(1000000, digits = 0), "1,000,000 EUR")
})

test_that("rounding happens before formatting", {
  expect_equal(pretty_price(1.005, digits = 2), pretty_price(round(1.005, 2), digits = 2))
})

test_that("extra ... arguments are passed through to formatC()", {
  # width is a formatC() argument, not one of pretty_price()'s own
  out <- pretty_price(5, digits = 0, width = 10)
  expect_true(grepl("5 EUR$", out))
  expect_gt(nchar(out), nchar(pretty_price(5, digits = 0)))
})
