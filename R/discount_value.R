#' A function to calculate the discounted value of a future costs or effects
#'
#' This function calculates the discounted value of a future costs or effects based on a discount rate and time period.
#'
#' @param current_value The current value.
#' @param discount_rate The discount rate to use for the calculation. Default is 0.03. The guideline stipulates 0.03 for costs and 0.015 for effects.
#' @param time The time at which the future value occurs.
#' @param time_unit The unit of time to use for the calculation. Default is "years", but "months", "weeks", and "days" are also valid options.
#' @param discount_year_one Logical value indicating whether to discount the first year as well. Default is FALSE.
#' @return A numeric value of the discounted future value.
#' @examples
#' discount_value(current_value = 100, discount_rate = 0.03, time = 5, time_unit = "years", discount_year_one = FALSE)
#' @export discount_value

discount_value <- function(
    current_value,
    discount_rate = 0.03,
    time,
    time_unit = c("years", "months","weeks", "days"),
    discount_year_one = FALSE){

  # match.arg() for the time_unit parameter to ensure it is one of the valid choices
  time_unit <- match.arg(time_unit)

  # Input validation with assertthat
  assertthat::assert_that(is.numeric(current_value), msg = "`current_value` must be numeric")
  assertthat::assert_that(is.numeric(discount_rate), msg = "`discount_rate` must be numeric")
  assertthat::assert_that(discount_rate >= 0 && discount_rate <= 1, msg = "`discount_rate` must be between 0 and 1")
  assertthat::assert_that(is.numeric(time), msg = "`time` must be numeric")
  assertthat::assert_that(time > 0, msg = "`time` must be greater than 0")
  assertthat::assert_that(time_unit %in% c("years", "months", "weeks", "days"),
              msg = "`time_unit` must be one of 'years', 'months', 'weeks', or 'days'")
  assertthat::assert_that(is.logical(discount_year_one), msg = "`discount_year_one` must be a logical value (TRUE or FALSE)")


  v_discount <- discount_vector(discount_rate = discount_rate,
                  start_time = 0, end_time = time, time_unit = time_unit,
                  discount_year_one = discount_year_one)

  v_discount[length(v_discount)] * current_value
}
