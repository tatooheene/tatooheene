#' A function to obtain the Dutch PPP factor values
#'
#' This function downloads the Purchasing Power Parity (PPP) factor values for the Netherlands from the OECD website per year.
#'
#' @param year The year of which the PPP factor should be downloaded, multiple years are possible, default is the whole dataset.
#' @keywords Generic, Costing Manual, Purchasing Power Parity (PPP)
#' @return A dataframe or value with the PPP factor values for the specified years.
#' @examples
#' nl_ppp(year = 2019)
#' @export nl_ppp

nl_ppp <- function(
    year = "all"){

  # Filter the data for the specified years
  update_year <- as.numeric(format(Sys.Date(), "%Y")) - 1
  new_filepath_ppp <- paste0("data/df_ppp_", update_year, ".rds")

  df_ppp <- readRDS(new_filepath_ppp)

  # Select the specified years, or all years if not specified
  if(year != "all"){
    ppp <- df_ppp |>
      dplyr::filter(Year %in% year) |>
      dplyr::pull(PPP)

    print(ppp)
  }else{

    return(df_ppp)
  }
}
