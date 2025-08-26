#' Calculate the discounted (net present value) of a stream of costs or effects
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function calculates the net present value of a vector of future costs or effects using constant discounting, as described in the Dutch guideline for economic evaluations in healthcare (section 2.6.1.2 version 2024).
#'
#' @param values        A numeric vector of costs or effects over time (one value per period).
#' @param discount_rate A categorical indication if the values are costs or health effects. The function uses the numeric discount rates for the Dutch guidelines accordingly (e.g. 0.03 for 3% of costs and 0.015 for 1.% for health effects). Default is 0.03.
#' @param discount_year_one Logical: should the first year (t = 0) be discounted? Default is FALSE.
#' @param aggregate Logical: should the stream be aggregated? Default is FALSE.

#' @return A numeric vector: the discounted values of the stream.
#' @examples
#' # Constant cost of 100 for 3 years, no discounting in year 0
#' discount_stream(values = rep(100, 3), discount_rate = 0.03, discount_year_one = FALSE)
#'
#' @export discount_stream

discount_stream <- function(values,
                            discount_rate = c("costs", "effects"),
                            discount_year_one = FALSE,
                            aggregate = FALSE) {

  # Validate and convert discount rate
  if (is.character(discount_rate)) {
    discount_rate <- match.arg(discount_rate)
    discount_rate <- switch(discount_rate,
                            costs   = 0.03,
                            effects = 0.015)
  }

  # Validation
  assertthat::assert_that(is.numeric(values),                       msg = "`values` must be numeric")
  assertthat::assert_that(is.numeric(discount_rate),                msg = "`discount_rate` must be numeric")
  assertthat::assert_that(discount_rate >= 0 && discount_rate <= 1, msg = "`discount_rate` must be between 0 and 1")
  assertthat::assert_that(is.logical(discount_year_one),            msg = "`discount_year_one` must be a logical value (TRUE or FALSE)")

  # Time vector (t = 0 for the first value if not discounting year one)
  time <- seq_along(values) - ifelse(discount_year_one, 0, 1)

  # Apply discounting
  discounted_values <- values * (1 + discount_rate)^(-time)

  if(aggregate == TRUE){
    discounted_values <- sum(discounted_values)

  }

  # Return
  round(discounted_values, 2)
}
