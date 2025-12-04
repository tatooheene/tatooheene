#' Calculate the discounted (net present value) of costs or effects
#'
#'@description
#' `r lifecycle::badge("experimental")`
#' The apply_discounting function is designed to calculate the net present value of future costs or effects using a constant  discount rate, following the Dutch guidelines for economic evaluations in health care.  (section 2.6.1.2 version 2024). Here's a breakdown of how the function works:
#'@param values A numeric (vector of) costs or effects over time (one value per period).
#'@param discount_rate Specifies the discount rate to be used. It can be "costs", "effects" or a custom numeric value
#'   Acceptable values are:
#'   \itemize{
#'     \item \code{"costs"} — applies a 3\% (0.03) annual discount rate
#'     \item \code{"effects"} — applies a 1.5\% (0.015) annual discount rate
#'     \item A numeric value (e.g., \code{0.04}) — applies a custom annual discount rate
#'   }
#'@param times A numeric (vector of) time points indicating the time used for the discounting. The length must match the length of the values vector. Since the default discounting is annual, the time points should be in years. The length of this vector should be the same as the length of the `values` vector. When the first year is not discounted, the time points should start at 0 (e.g., c(0, 1, 2) for three years with NO discounting in the first year). When the first year is discounted, the time points should start at 1 (e.g., c(1, 2, 3) for three years WITH discounting in the first year). In case costs or effects are accrued in time steps other they annual, the time points should be adjusted accordingly, see more details in the vignettes of this package about discounting.
#'@param aggregate A logical: indicating whether to sum the discounted values. Default is FALSE.
#'@param digits A numeric value to indicate the number of digits to round the value. Default is 3 digits
#'
#'
#'@examples
#' # NO Discounting in First Year (t starts at 0)
#' constant cost of 100 for 3 years
#' apply_discounting(values = rep(100, 3), discount_rate = "costs", times = c(0, 1, 2))
#'
#' # WITH discounting in first year (t starts at 1)
#' example: Constant cost of 100 for 3 years,
#'  apply_discounting(values = rep(100, 3), discount_rate = "costs", times = c(1, 2, 3))
#'
#' # Present value of 100 euro in 3 years
#'  apply_discounting(values = 100, discount_rate = "costs", times = 3)
#'
#' # Custom Discount Rate
#' # example: discount rate of 4%, no discounting in first year
#' apply_discounting(values = rep(100, 3), discount_rate = 0.04, times = c(0, 1, 2))
#'#' This will give you a messages to inform you about the different discount rate
#'
#' Same applies to utility values
#' Utility values with aggregation - NO discounting in first year
#' apply_discounting(values = c(0.98, 0.82, 0.79), discount_rate = "effect", times = c(0, 1, 2), aggregate = TRUE, digits = 3)
#'
#'@export apply_discounting
#'@details
#' This function ensures consistent application of discount rates in cost-effectiveness
#' analyses, in line with Dutch guidelines. Custom rates can be specified when needed.
#'

apply_discounting <- function(values,
                              discount_rate = c("costs", "effects"),
                              times,
                              aggregate = FALSE,
                              digits = NULL) {

  # Validate and convert discount rate
  if (is.character(discount_rate)) {
    discount_rate <- match.arg(discount_rate)
    discount_rate <- switch(discount_rate,
                            costs   = 0.03,  # discount rates for costs  of Dutch costing manual
                            effects = 0.015) # discount rates for effects of Dutch costing manual
  }

  # Show a message in case the user uses or explores a discount rate different than the Dutch costing manual
  msg <- assertthat::validate_that(
    discount_rate == 0.03 | discount_rate == 0.015,
    msg = "The used `discount_rate` is different than the one recommended in the Dutch guidelines"
  ) # Note, there is no warning in case a users uses the discount rates for effects for costs (or the other way around)

  if (!isTRUE(msg)) {
    message(msg)
  }

  # Convert to vectors if they are matrices
  if (is.matrix(values)) {
    values <- as.vector(values)
  }
  if (is.matrix(times)) {
    time <- as.vector(times)
  }

  # Check both are vectors
  if (!is.vector(values) || !is.vector(times)) {
    stop("Both inputs must be vectors or convertible to vectors.")
  }

  # Check they are the same length
  if (length(values) != length(times)) {
    stop("Values and time must be of the same length.")
  }

  # Validation
  assertthat::assert_that(is.numeric(values),                       msg = "`values` must be numeric")
  assertthat::assert_that(is.numeric(discount_rate),                msg = "`discount_rate` must be numeric")
  assertthat::assert_that(discount_rate >= 0 && discount_rate <= 1, msg = "`discount_rate` must be between 0 and 1")


  t      <- times # save the time for the time vector

  #  Calculate the discount factor at the time point of interest
  v_discount_weights <- 1 * (1 + discount_rate)^(-t) # vector discount weights
  # Note - this is the formula based on the equation in the Dutch guidelines.
  # The more common form to write this formula is PV = value / (1 + r) ^ t, with
  # PV: present value . The results however are identical. This would look like:
  # v_discount_weights <- 1 / (1 + discount_rate)^(t)

  # Apply discounting
  discounted_values <- values * v_discount_weights

  # Sum the results
  if(aggregate == TRUE){
    discounted_values <- sum(discounted_values)}

  # Only round if user specifies the digits argument
  if (!is.null(digits)) {
    discounted_values <- round(discounted_values, digits)
  }

  return(discounted_values)   # Return

}


