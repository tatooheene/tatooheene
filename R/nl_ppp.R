#' A function to obtain the Dutch PPP factor values in International Dollar (INT$)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function downloads the Purchasing Power Parity (PPP) factor values for the Netherlands from the OECD website per year in International Dollar (Int$).
#'
#' @param year The year of which the PPP factor should be downloaded, multiple years are possible, default is the whole dataset.
#' @keywords Generic, Costing Manual, Purchasing Power Parity (PPP)
#' @return A dataframe or value with the PPP factor values for the specified years.
#' @examples
#' # Example usage of the nl_ppp function
#' nl_ppp(year = 2019)
#' @export nl_ppp

nl_ppp <- function(
    year = "all"){

  # Read in the dataset
  df <- tatooheene::df_ppp

  # Select the specified years, or all years if not specified
  if(year != "all"){
    ppp <- df |>
      dplyr::filter(Year %in% year) |>
      dplyr::pull(PPP)

    return(ppp)
  }else{

    return(df)
  }
}
