test_that("default ('all') returns the full dataset", {
  out <- nl_ref_prices()
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), nrow(tatooheene::df_ref_prices))
})

test_that("single year + category returns a long-format tibble", {
  out <- nl_ref_prices(year = "2024", category = "Nursing")
  expect_true(is.data.frame(out))
  expect_true(all(out$Category == "Nursing"))
  expect_true(all(out$Year == "2024"))
})

test_that("multiple years are accepted", {
  out <- nl_ref_prices(year = c(2022, 2023), category = "Nursing")
  expect_true(all(out$Year %in% c("2022", "2023")))
  expect_setequal(unique(out$Year), c("2022", "2023"))
})

test_that("a single matching row returns a bare numeric instead of a data frame", {
  one_unit <- unique(tatooheene::df_ref_prices$Unit)[1]
  out <- nl_ref_prices(year = "2024", unit = one_unit)
  expect_type(out, "double")
  expect_length(out, 1L)
})

test_that("currency = 'INT$' converts prices (differs from the EUR default)", {
  eur <- nl_ref_prices(year = "2022", category = "Nursing", currency = "EUR")
  int <- nl_ref_prices(year = "2022", category = "Nursing", currency = "INT$")
  expect_false(isTRUE(all.equal(eur$Price, int$Price)))
})

test_that("an invalid year errors with a clear message", {
  expect_error(nl_ref_prices(year = "1900"), "Invalid year")
})

test_that("an invalid category errors clearly (match.arg validation)", {
  expect_error(nl_ref_prices(category = "not-a-real-category"))
})

test_that("an invalid unit errors clearly (match.arg validation)", {
  expect_error(nl_ref_prices(unit = "not-a-real-unit"))
})

# NOTE: `domain`/`short_unit` currently do NOT validate their input the way
# `category`/`unit` do (an invalid value silently returns 0 rows instead of
# erroring), and `category`/`unit` currently do NOT accept multiple values
# despite their own docs saying "one or more" is supported. Both are tracked
# as a known issue (see tatooheene-improve/github-issues/task-07-*.md) with
# a fix + regression tests already drafted there — intentionally not
# asserted here to avoid this file locking in the current, known-incorrect
# behavior as "correct".
