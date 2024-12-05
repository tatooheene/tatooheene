#' A function to download the Medical Reference prices of the Dutch Costing Manual for one or multiple years
#'
#' This function downloads the Medical Reference prices of the Dutch Costing Manual for one or multiple years. The prices are available in Euro (EUR) or International Dollar (INT$).
#'
#' @param year The year of which the reference price should be downloaded, multiple years are possible, default is the whole dataset
#' @param category The category of prices that should be included (one or more categories), default is including all categories
#' @param unit The reference price that should be included (one or multiple reference prices),  default is including the whole dataframe
#' @param currency The currency of the output of the prices. A decision can be made between EUR and INT$, the default is EUR.
#' @return A dataframe with the Medical Reference prices of the Dutch Costing Manual for the specified years.
#' @examples
#' Example usage of the nl_med_prices function
#' nl_med_prices(year = 2019, category = "Nursing", unit = "Nursing day excluding personnel costs, hospital")
#' @keywords Generic, Costing Manual, Dutch Reference Prices, Medical Prices
#' @export nl_med_prices

nl_med_prices <- function(
    year = "all",
    category = "all",
    currency = "EUR",
    unit = "all"){

  # read in the dataset
  df <- df_rp_medical

  # If specified, select the specified years, or all years if not specified
  if(year != "all"){
    df <-  df |>
      dplyr::select("Category", "Unit", all_of(c(as.character(year))))
  }

  #If currency is INT$, change output
  if(currency == "INT$"){
    df_ppp <- tatooheene::nl_ppp()
    df_ppp <- df_ppp |>
      dplyr::filter(as.numeric(Year) >= 2022) |>
      tidyr::pivot_wider(names_from = "Year", values_from = "PPP")

    df_ppp <- df_ppp[, rev(seq_len(ncol(df_ppp)))]

    common_years <- intersect(colnames(df), colnames(df_ppp))

    df[, common_years] <- sweep(df[, common_years], 2, as.numeric(df_ppp[, common_years]), `*`)
  }


  # If specified, filter based on category
  if(category != "all"){
    df <- df |>
      dplyr::filter(Category %in% category)
  }

  # If specified, filter based on unit
  if(unit != "all"){
    df <- df |>
      dplyr::filter(Unit %in% unit)
  }

  return(df)
}
