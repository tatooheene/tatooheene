#' Annual depreciation + interest for medical equipment
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Compute the annuity factor and the annual depreciation-and-interest charge for medical equipment, following Section 7.3 of the Dutch costing manual; k = annual depreciation and interest expense jaarlijkse afschrijvings- en rentekosten
#'
#' Let V be replacement value, R the salvage value, n the amortisation period (years), and i the interest rate (per year). The annuity factor is:
#' \deqn{a_{n,i} = \frac{1}{i}*\bigg(1-\frac{1}{(1 + i)^{n}}\bigg)}
#' #'
#' The annual charge k is:
#' \deqn{k = \frac{V - \frac{R}{(1 + i)^n}}{a_{n,i}}}
#'
#' @param v_replace_val V: vervangingswaarde; replacement value (numeric scalar, > 0)
#' @param r_salvage_val R: restwaarde; salvage (residual) value at end of period (numeric scalar, >= 0)
#' @param n_amortisation_period n: afschrijvingstermijn; amortisation period in years (numeric scalar, > 0)
#' @param i_interest_rt i: rentepercntage; annual interest rate as a decimal (numeric scalar, >= 0)
#' @param output One of `dataframe` (default), `annuity_factor`, or `annual_cost`.
#'
#' @return
#' - If `output = "dataframe"`: a data.frame with two columns:
#'  `Annuity factor` and `Yearly depreciation and interest costs`.
#' - If `output = "annuity_factor"`: a single numeric (the annuity factor).
#' - If `output = "annual_cost"`: a single numeric (the annual charge k).
#'
#' @keywords Generic, costs equipment
#' @examples
#' # Both values as a data frame (defaults: n=10, i=2.5%)
#' depreciation_interest(v_replace_val = 50000, r_salvage_val = 5000)
#'
#' # Only the annuity factor
#' depreciation_interest(50000, 5000, output = "annuity_factor")
#'
#' # Only the annual charge (k)
#' depreciation_interest(50000, 5000, output = "annual_cost")
#'
#' # Zero interest (uses the i -> 0 limit): a = n, k = (V - R)/n
#' depreciation_interest(50000, 5000, n_amortisation_period = 8, i_interest_rt = 0, output = "dataframe")
#'
#' @export depreciation_interest

depreciation_interest <- function(
    v_replace_val,
    r_salvage_val,
    n_amortisation_period = 10,
    i_interest_rt = 0.025,
    output = c("dataframe", "annuity_factor", "annual_cost")
) {
  output <- match.arg(output)

  # ---- input checks (no extra deps) ----
  if (!is.numeric(v_replace_val) || length(v_replace_val) != 1L || is.na(v_replace_val) || v_replace_val <= 0) {
    stop("`v_replace_val` must be a single positive number.", call. = FALSE)
  }
  if (!is.numeric(r_salvage_val) || length(r_salvage_val) != 1L || is.na(r_salvage_val) || r_salvage_val < 0) {
    stop("`r_salvage_val` must be a single non-negative number.", call. = FALSE)
  }
  if (!is.numeric(n_amortisation_period) || length(n_amortisation_period) != 1L ||
      is.na(n_amortisation_period) || n_amortisation_period <= 0) {
    stop("`n_amortisation_period` must be a single positive number (years).", call. = FALSE)
  }
  if (!is.numeric(i_interest_rt) || length(i_interest_rt) != 1L || is.na(i_interest_rt) || i_interest_rt < 0) {
    stop("`i_interest_rt` must be a single non-negative number (e.g., 0.025 for 2.5%).", call. = FALSE)
  }
  if (r_salvage_val > v_replace_val) {
    warning("`r_salvage_val` is greater than `v_replace_val`. Is that intended?")
  }

  V <- v_replace_val
  R <- r_salvage_val
  n <- n_amortisation_period
  i <- i_interest_rt

  # ---- annuity factor with safe zero-interest branch ----
  if (i == 0) {
    a_annuity_fct <- n
  } else {
    a_annuity_fct <- (1 - (1 + i)^(-n)) / i
  }

  # ---- annual depreciation + interest ----
  k_annual_depr_int_exp <- (V - R / (1 + i)^n) / a_annuity_fct

  if (output == "annuity_factor") {
    return(unname(a_annuity_fct))
  }
  if (output == "annual_cost") {
    return(unname(k_annual_depr_int_exp))
  }

  out <- data.frame(
    "Annuity factor" = a_annuity_fct,
    "Yearly depreciation and interest costs" = k_annual_depr_int_exp,
    check.names = FALSE
  )
  out
}
