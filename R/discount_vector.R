#' A function to create a discount vector for a given time period
#'
#'@description
#' `r lifecycle::badge("experimental")`
#' This function calculates a discount vector for a given time period based on the in paragraph 2.6.1.2 of the Dutch EE guideline mentioned discount rate and time period
#'
#' @param discount_rate The discount rate to use for the calculation. Default is 0.03. The guideline stipulates 0.03 for costs and 0.015 for effects.
#' @param start_time The start time for the discount vector. Default is 0.
#' @param end_time The end time for the discount vector.
#' @param time_unit The unit of time to use for the calculation. Default is "years", but "months", "weeks", and "days" are also valid options.
#' @param discount_year_one Logical value indicating whether to discount the first year as well. Default is FALSE.
#' @return A numeric vector of discounted values for each time period.
#' @keywords Discounting, Costs, Effects
#' @examples
#' # Example usage of the discount_vector function
#' # Calculate the discount vector for 5 years, a discount rate of 0.015, first year is not discounted
#' discount_vector(discount_rate = 0.015, end_time = 5, time_unit = "years")
#'
#' # Calculate the discount vector for 60 months, a start time of 2, the first year is not discounted
#' discount_vector(discount_rate = 0.03, start_time = 2, end_time = 60, time_unit = "months")
#'
#' # Calculate the discount vector for 365 days, a discount rate of 0.06, the first year is discounted
#' discount_vector(discount_rate = 0.06, end_time = 365, time_unit = "days", discount_year_one = TRUE)
#'
#' @export discount_vector

discount_vector <- function(
    discount_rate = 0.03,
    start_time = 0,
    end_time,
    time_unit = c("years", "months", "weeks", "days"),
    discount_year_one = FALSE){

  # match.arg() for the time_unit parameter to ensure it is one of the valid choices
  time_unit <- match.arg(time_unit)

  # Input validation with assertthat
  assertthat::assert_that(is.numeric(discount_rate), msg = "`discount_rate` must be numeric")
  assertthat::assert_that(discount_rate >= 0 && discount_rate <= 1, msg = "`discount_rate` must be between 0 and 1")
  assertthat::assert_that(is.numeric(start_time), msg = "`start_time` must be numeric")
  assertthat::assert_that(start_time >= 0, msg = "`start_time` must be greater than or equal to 0")
  assertthat::assert_that(is.numeric(end_time), msg = "`end_time` must be numeric")
  assertthat::assert_that(end_time > 0, msg = "`end_time` must be greater than 0")
  assertthat::assert_that(time_unit %in% c("years", "months", "weeks", "days"),
              msg = "`time_unit` must be one of 'years', 'months', 'weeks', or 'days'")
  assertthat::assert_that(is.logical(discount_year_one), msg = "`discount_year_one` must be a logical value (TRUE or FALSE)")

  if(time_unit == "years" && (end_time - start_time) %% 1 != 0){
    warning("Time is not a whole number of years. Discount vector will only be calculated for whole years. Consider using a different time (e.g., months) unit for more precise calculations.")
    }

  # Select time factor based on time unit
  switch(time_unit,
         years = time_factor <- 1,
         months = time_factor <- 12,
         weeks = time_factor <- 52,
         days = time_factor <- 365)

  # Discount rate in unit of time
  discount_rate_time <- (1 + discount_rate) ^ (1 / time_factor) - 1

  # Calculate discount vector with discounting from start
  v_discount <- 1 / (1 + discount_rate_time) ^ seq(start_time, end_time, by = 1)

  # If discount_year_one is TRUE, prepend a 1 to the discount vector
  if(!discount_year_one){
    v_discount <- c(rep(1, times = length(1:time_factor)),
                    v_discount)[1:end_time]
  }

  # Return discount vector
  v_discount
}
