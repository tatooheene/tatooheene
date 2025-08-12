test_that("defaults return a tibble with Year + weeks_5yr", {
  out <- friction_period()
  expect_s3_class(out, "tbl_df")
  expect_true("Year" %in% names(out))
  # should include the 5yr weeks column
  expect_true("Friction period weeks average over 5 years" %in% names(out))
})

test_that("single value mode works", {
  yr  <- min(tatooheene::df_fp$Year, na.rm = TRUE) + 1L
  val <- friction_period(year = yr, units = "weeks", avg = "5yr", output = "value")
  expect_type(val, "double")
  expect_length(val, 1L)
  # sanity: equals the table cell
  ref <- subset(tatooheene::df_fp, Year == yr)[["Friction period weeks average over 5 years"]]
  expect_equal(val, as.numeric(ref))
})

test_that("year range is validated", {
  max_year <- max(tatooheene::df_fp$Year, na.rm = TRUE)
  expect_error(friction_period(year = max_year + 5), "Year out of range")
})

test_that("multiple selections are returned as columns", {
  yrs <- head(tatooheene::df_fp$Year, 3L)
  out <- friction_period(year = yrs, units = c("days","weeks"), avg = "1yr")
  expect_true(all(c("Friction period in days", "Friction period in weeks") %in% names(out)))
  expect_equal(nrow(out), length(yrs))
})
