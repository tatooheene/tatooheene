#' A function to calculate the costs of medical equipment
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' A function to calculate the costs of medical equipment based on Section 3.3 of the Dutch EE guideline; k = annual depreciation and interest expense jaarlijkse afschrijvings- en rentekosten
#'
#' @param v_replace_val V: vervangingswaarde; replacement value
#' @param r_salvage_val R: restwaarde; salvage value
#' @param n_amortisation_period N: afschrijvingstermijn,; amortization period
#' @param i_interest_rt i: renteperceof ntage; interest rate
#' @param output Default of output is a data frame with both the annuity factor and yearly deprecation and interest costs, but the values can be selected independently
#' @return A data frame with the annuity factor, yearly depreciation and interest costs, or the values independently.
#' @keywords Generic, costs equipment
#' @examples
#' # Example usage of the depreciation_interest function
#' # Calculate both annuity factor and yearly depreciation and interest costs as a data frame
#' depreciation_interest(v_replace_val = 50000, r_salvage_val = 5000)
#'
#' # Get only the annuity factor
#' depreciation_interest(v_replace_val = 50000, r_salvage_val = 5000, output = "annuity factor")
#'
#' # Get only the annual depreciation and interest cost
#' depreciation_interest(v_replace_val = 50000, r_salvage_val = 5000, output = "annual cost")
#'
#' @export depreciation_interest

depreciation_interest <- function(
    v_replace_val,
    r_salvage_val,
    n_amortisation_period = 10,
    i_interest_rt = 0.025,
    output = c("data frame", "annuity factor", "annual cost")){

  # match.arg() for the output parameter to ensure it is one of the valid choices
  output <- match.arg(output)

  # Input validation with assertthat
  assertthat::assert_that(is.numeric(v_replace_val), msg = "`v_replace_val` must be numeric")
  assertthat::assert_that(is.numeric(r_salvage_val), msg = "`r_salvage_val` must be numeric")
  assertthat::assert_that(is.numeric(n_amortisation_period), msg = "`n_amortisation_period` must be numeric")
  assertthat::assert_that(n_amortisation_period > 0, msg = "`n_amortisation_period` must be greater than 0")
  assertthat::assert_that(is.numeric(i_interest_rt), msg = "`i_interest_rt` must be numeric")
  assertthat::assert_that(i_interest_rt >= 0 && i_interest_rt <= 1, msg = "`i_interest_rt` must be between 0 and 1")
  assertthat::assert_that(output %in% c("data frame", "annuity factor", "annual cost"),
              msg = "`output` must be one of 'DataFrame', 'annuity_fct', or 'annual_cost'")

  # Annuity factor
  a_annuity_fct <- (1 / i_interest_rt) * (1 - (1 / (1 + i_interest_rt)^n_amortisation_period))

  # Yearly depreciation and interest costs
  k_annual_depr_int_exp <- (v_replace_val - (r_salvage_val /
                                               (1 + i_interest_rt)^n_amortisation_period)
  ) / (a_annuity_fct)

  # If parameter output = data frame
  if(output == "data frame"){
    out <- data.frame(a_annuity_fct, k_annual_depr_int_exp) |>
      dplyr::rename("Annuity factor" = a_annuity_fct,
                    "Yearly depreciation and interest costs" = k_annual_depr_int_exp)

    return(out)

    # if parameter output = "annuity factor"
  }else if(output == "annuity factor"){
    print(a_annuity_fct)

    # if parameter output = annual cost (Yearly depreciation and interest costs)
  }else if(output == "annual cost"){
    print(k_annual_depr_int_exp)
  }
}
