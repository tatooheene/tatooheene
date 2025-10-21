#' Calculate the discounted (net present value) of costs or effects
#'
#'#' `r lifecycle::badge("experimental")`
#' This function calculates the net present value of future costs or effects using constant discounting, as described in the Dutch guideline for economic evaluations in healthcare (section 2.6.1.2 version 2024).
#'
#' @param values  A numeric (vector of) costs or effects over time (one value per period).
#' @param discount_rate A categorical indication if the values are costs or health effects. The function uses the numeric annual discount rates for the Dutch guidelines accordingly (e.g. 0.03 for 3% of costs and 0.015 for 1.5% for health effects). Default is 0.03.
#' @param times   A numeric (vector of) time points indicating the time used for the discounting

#' @return A numeric vector: the discounted values of the stream (with aggregated = FALSE) or the total discounted sum (when aggregated = TRUE)
#'
#'
#'
#' @examples
#' # Constant cost of 100 for 3 years, no discounting in first year
#' apply_discounting(values = rep(100, 3), discount_rate = "costs", times = c(0, 1, 2))
#'
#' # Constant cost of 100 for 3 years, WITH discounting in fist year 1
#'  apply_discounting(values = rep(100, 3), discount_rate = "costs", times = c(1, 2, 3))
#'
#' # Explore a different discount rate of 4%, no discounting in first year
#' apply_discounting(values = rep(100, 3), discount_rate = 0.04, times = c(0, 1, 2))
#'
#' # Explore the present value of 100 euro in 3 years
#'apply_discounting(values = 100, discount_rate = "costs", times = 3)
#'
#' Same applies to utility values
#'
#'#' @export discount
#'
#'
#'
#'@STIJN : do we like to have a default for times - in case time is not specified? Assum annual? Or that is to risky and trick? - Function aggregate data is now removed, as we had previously? Or shall I add this back into the function?


apply_discounting <- function(values,
                     discount_rate = c("costs", "effects"),
                     times) {

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


  # Validation
  assertthat::assert_that(is.numeric(values),                       msg = "`values` must be numeric")
  assertthat::assert_that(is.numeric(discount_rate),                msg = "`discount_rate` must be numeric")
  assertthat::assert_that(discount_rate >= 0 && discount_rate <= 1, msg = "`discount_rate` must be between 0 and 1")


  t      <- times # save the time for the time vector

  # Apply discounting
  discounted_values <- values * (1 + discount_rate)^(-t)

  return(discounted_values)

}


