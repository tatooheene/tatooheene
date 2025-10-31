#' Calculate the discounted (net present value) of a stream of costs or effects
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function calculates the net present value of a vector of future costs or effects using constant discounting, as described in the Dutch guideline for economic evaluations in healthcare (section 2.6.1.2 version 2024).
#'
#' @param values        A numeric vector of costs or effects over time (one value per period).
#' @param discount_rate A categorical indication if the values are costs or health effects. The function uses the numeric annual discount rates for the Dutch guidelines accordingly (e.g. 0.03 for 3% of costs and 0.015 for 1.5% for health effects). Default is 0.03.
#' @param discount_year_one Logical: should the first cycle (t = 0) be discounted? Default is FALSE.
#' @param cycle_length_in_years   A numeric argument to indicate of the duration of a cycle length in fraction of a year. For annual cycles, use 1; for monthly 1/12; for weekly 1/52.
#' @param aggregate Logical: should the stream be aggregated? Default is FALSE.

#' @return A numeric vector: the discounted values of the stream (with aggregated = FALSE) or the total discounted sum (when aggregated = TRUE)
#' @examples
#' # Constant cost of 100 for 3 years, no discounting in year 0
#' discount_stream(values = rep(100, 3), discount_rate = "costs", discount_year_one = FALSE)
#'
#' # Constant cost of 100 for 3 years, no discounting in year 0 and give aggregated results
#' discount_stream(values = rep(100, 3), discount_rate = "costs", discount_year_one = FALSE, aggregate = TRUE)
#'
#' # Explore a different discount rate of 4%
#' discount_stream(values = rep(100, 3), discount_rate = 0.04, discount_year_one = FALSE)

#' # Example: Average QALY gain for four years to be discounted
#' discount_stream(values = c(1, 0.98, 0.68, 0.64), discount_rate = "effects", discount_year_one = FALSE)

#' # Monthly QALYs for 1 year
#' discount_stream(rep(1/12, 12), discount_rate = "effects", cycle_length_in_years = 1/12)




##ISSUE with discount FIRST YEAR, or first cycle!
# Discount YEAR one FALSE means now only do not discount first cycle

#' @export discount_stream

discount_stream <- function(values,
                            discount_rate = c("costs", "effects"),
                            discount_year_one = FALSE, #Q: shall we rename to discount_cycle_one??
                            cycle_length_in_years = 1,
                            aggregate = FALSE,
                            digits = 3 ) {

  # Validate and convert discount rate
  if (is.character(discount_rate)) {
    discount_rate <- match.arg(discount_rate)
    discount_rate <- switch(discount_rate,
                            costs   = 0.03,  # discount rates for costs of Dutch costing manual
                            effects = 0.015) # discount rates for effects of Dutch costing manual
  }

  # Show a message in case the user uses or explores a discount rate different than the Dutch costing manual
  msg <- assertthat::validate_that(
    discount_rate == 0.03 | discount_rate == 0.015,
    msg = "The used `discount_rate` is different then the one recommend in the Dutch guidelines"
  )

  if (!isTRUE(msg)) {
    message(msg)
  }


  # Show a message in case the user uses or explores a discount rate different than the Dutch costing manual
  msg <- assertthat::validate_that(
    cycle_length_in_years == 1,
    msg = "The used cycle length is different from a year. This means that the argument `discount_year_one` means: Should we discount the first cycle? All other cycles are discounted. See vignettes about how to deal with ways where you do not like to discount any of the reward accumulated in the first year."
  )

  if (!isTRUE(msg)) {
    message(msg)
  }




  # Validation
  assertthat::assert_that(is.numeric(values),                                     msg = "`values` must be numeric")
  assertthat::assert_that(is.numeric(cycle_length_in_years),                      msg = "`cycles_length_in_years` must be numeric")
  assertthat::assert_that(is.logical(discount_year_one),                          msg = "`discount_year_one` must be a logical value (TRUE or FALSE)")
  assertthat::assert_that(is.numeric(discount_rate),                              msg = "`discount_rate` must be numeric")
  assertthat::assert_that(discount_rate >= 0 && discount_rate <= 1,               msg = "`discount_rate` must be between 0 and 1")
  assertthat::assert_that(cycle_length_in_years >= 0 && cycle_length_in_years <= 1, msg = "`discount_rate` must be between 0 and 1")
  assertthat::assert_that(is.logical(aggregate),                                  msg = "`aggregate` must be a logical value (TRUE or FALSE)")


  # Time vector (t = 0 for the first value if not discounting year one)
  offset <- ifelse(discount_year_one, 0, 1)
  t      <- (seq_along(values) - offset) / cycle_length_in_years


  # Apply discounting
  discounted_values <- values * (1 + discount_rate)^(-t)

  if(aggregate == TRUE){
    discounted_values <- sum(discounted_values)
    discounted_values <- round(discounted_values, digits) # round values to digits
}

  # Return
  round(discounted_values, digits)  # round values to digits


}
