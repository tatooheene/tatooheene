#' A function to download the Productivity and other societal Reference prices of the Dutch Costing Manual for one or multiple years
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function downloads the Productivity and other societal Reference prices of the Dutch Costing Manual for one or multiple years. The prices are available in Euro (EUR) or International Dollar (INT$).
#'
#' @param year The year of which the reference price should be downloaded, multiple years are possible, default is the whole dataset
#' @param category The category of prices that should be included (one or more categories), default is including all categories
#' @param unit The reference price that should be included (one or multiple reference prices),  default is including the whole dataframe
#' @param currency The currency in which the reference price should be included (EUR or INT$), default is EUR
#' @return A dataframe or value with the Productivity and/other societal Reference price(s) of the Dutch Costing Manual
#' @examples
#' # Example usage of the nl_prod_oth_prices function:
#' # Calculate for 2023 with the category Productivity loss - Paid work in EUR
#' nl_prod_oth_prices(year = "2023", category = "Productivity loss - Paid work")
#'
#' # Calculate for 2022 and 2023 with the category Productivity loss - Paid work in EUR
#' nl_prod_oth_prices(year = "all", category = "Productivity loss - Paid work")
#'
#' # Calculate for 2022 with the category Productivity loss - Paid work in INT$
#' nl_prod_oth_prices(year = "2022", category = "Productivity loss - Paid work", currency = "INT$")
#'
#' @keywords Generic, Costing Manual, Dutch Reference Prices, Patient & Family Prices, Productivity Prices, Other Societal Prices
#' @export nl_prod_oth_prices

nl_prod_oth_prices <- function(
    year = "all",
    category = "all",
    unit = "all",
    currency = c("EUR", "INT$")){

  # Read in the dataset
  df <- tatooheene::df_rp_prod

  # Input validation
  year <- match.arg(year, c("all",
                            suppressWarnings(na.omit(as.numeric(colnames(df))
                                                 ))))
  possible_cat <- c("all", unique(df$Category))
  category <- match.arg(category, possible_cat)

  currency <- match.arg(currency)

  possible_unit <- c("all", unique(df$Unit))
  unit <- match.arg(unit, possible_unit)


  #If currency is INT$, change output
  if(currency == "INT$"){
    df_ppp <- tatooheene::nl_ppp()
    df_ppp <- df_ppp |>
      dplyr::filter(as.numeric(Year) >= 2022) |>
      tidyr::pivot_wider(names_from = "Year", values_from = "PPP")

    df_ppp <- df_ppp[, rev(seq_len(ncol(df_ppp)))]

    common_years <- intersect(colnames(df), colnames(df_ppp))

    df[, common_years] <- sweep(df[, common_years], 2, as.numeric(df_ppp[, common_years]), `/`)
  }

  # If specifiec, filter based on category
  if(category != "all"){
    df <- df |>
      dplyr::filter(Category %in% category)
  }

  if(unit != "all"){
    df <- df |>
      dplyr::filter(Unit %in% unit)
  }

  if(year != "all"){
    df <-  df |>
      dplyr::select("Category", "Unit", dplyr::all_of(c(as.character(year))))
  }

  return(df)
}
