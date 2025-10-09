#' A function to calculate the discounted value of future costs or effects
#'
#'@description
#'`r lifecycle::badge("experimental")`
#' This function calculates the net discounted value of a future costs or effects based on the constant discounting model as presented in paragraph 2.6.1.2 of the Dutch Costing guideline (version 2024) and the recommended discount rate and time period
#'
#' @param value The value to be discounted.
#' @param discount_rate A categorical indication if the values are costs or health effects. The function uses the numeric discount rates for the Dutch guidelines accordingly (e.g. 0.03 for 3% of costs and 0.015 for 1.5% for health effects). Default is 0.03.
#' @param year The time at which the future value occurs in years. For other duration us fractions of a year, a month 1/12 and a day 1/365 etc.
#' @param discount_year_one Logical value indicating whether to discount the first year as well. Default is FALSE.
#' @return A numeric value of the discounted value.
#' @keywords Discounting, Costs, Effects
#' @examples
#' # Example usage of the discount_value function
#' # Calculate the discounted value of 100 after 3 years, the first year is not discounted
#' discount_value(value = 100, discount_rate = "costs", year = 3)
#'
#' @export discount_value

discount_value <- function(
    value,
    year,
    discount_rate = c("costs", "effects"),
    discount_year_one = FALSE,
    digits = 3 ){

  # Validate and convert discount rate
  if (is.character(discount_rate)) {
    discount_rate <- match.arg(discount_rate)
    discount_rate <- switch(discount_rate,
                            costs   = 0.03,
                            effects = 0.015)
  }

  # Show a message in case the user uses or explores a discount rate different than the Dutch costing manual
  msg <- assertthat::validate_that(
    discount_rate == 0.03 | discount_rate == 0.015,
    msg = "The used `discount_rate` is different from the one recommended in the Dutch guidelines"
  )

  if (!isTRUE(msg)) {
    message(msg)
  }



  # Validation
  assertthat::assert_that(is.numeric(value),             msg = "`value` must be numeric")
  assertthat::assert_that(is.numeric(year) && year >= 0, msg = "`year` must be a non-negative number")
  assertthat::assert_that(is.numeric(discount_rate),     msg = "`discount_rate` must be numeric")
  assertthat::assert_that(discount_rate >= 0 && discount_rate <= 1, msg = "`discount_rate` must be between 0 and 1")
  assertthat::assert_that(is.logical(discount_year_one), msg = "`discount_year_one` must be TRUE or FALSE")

  # Adjust year if first year is not discounted
  t <- if (discount_year_one) year else max(year - 1, 0)

  # Discounting
  discounted <- value * (1 + discount_rate)^(-t)

  round(discounted, digits)
}
