#' A function to download the Patient & Family Reference prices of the Dutch Costing Manual for one or multiple years
#'
#' This function downloads the Patient & Family Reference prices of the Dutch Costing Manual for one or multiple years. The prices are available in Euro (EUR) or International Dollar (INT$).
#'
#' @param year The year of which the reference price should be downloaded, multiple years are possible, default is the whole dataset (year = "all")
#' @param category The category of prices that should be included (one or more categories), default is including all categories
#' @param currency The currency of the output of the prices. A decision can be made between EUR and INT$, the default is EUR.
#' @param unit The reference price that should be included (one or multiple reference prices),  default is including the whole data frame
#' @return A dataframe or value with the Patient & Family Reference price(s) of the Dutch Costing Manual for the specified years
#' @examples
#' Example usage of the nl_pat_fam_prices function
#' nl_pat_fam_prices(year = 2019, category = "Transportation", unit = "Car, cost per kilometer")
#' @keywords Generic, Costing Manual, Dutch Reference Prices, Patient & Family Prices
#' @export nl_pat_fam_prices

nl_pat_fam_prices <- function(
    year = "all",
    category = "all",
    currency = "EUR",
    unit = "all"){

  # Read dataframe
  df <- tatooheene::df_rp_patient

  if(year != "all"){
    df <-  df |>
      dplyr::select("Category", "Unit", dplyr::all_of(c(as.character(year))))
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

  if(unit != "all"){
    df <- df |>
      dplyr::filter(Unit %in% unit)
  }

  return(df)
}
