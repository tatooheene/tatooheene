#' A function to download the Patient & Family Reference prices of the Dutch Costing Manual for one or multiple years
#' @param year The year of which the reference price should be downloaded, multiple years are possible, default is the whole dataset (year = "all")
#' @param category The category of prices that should be included (one or more categories), default is including all categories
#' @param unit The reference price that should be included (one or multiple reference prices),  default is including the whole data frame
#' @keywords Generic, Costing Manual, Dutch Reference Prices, Patient & Family Prices
#' @export nl_pat_fam_prices

nl_pat_fam_prices <- function(
    year = "all",
    category = "all",
    currency = "EUR",
    unit = "all"){

  # Filepath
  ##  Get system year - 1 year for update
  update_year <- as.numeric(format(Sys.Date(), "%Y")) - 1
  new_filepath_patient <- paste0("data/RP_patient_", update_year, ".rds")

  # Read dataframe
  df_patient <- readRDS(new_filepath_patient)

  if(year != "all"){
    df_patient <-  df_patient |>
      dplyr::select("Category", "Unit", all_of(c(as.character(year))))
  }

  #If currency is INT$, change output
  if(currency == "INT$"){
    df_ppp <- f_factor_EurDutch_to_IntDollar()
    df_ppp <- df_ppp |>
      dplyr::filter(as.numeric(Year) >= 2022) |>
      tidyr::pivot_wider(names_from = "Year", values_from = "PPP")

    df_ppp <- df_ppp[, rev(seq_len(ncol(df_ppp)))]

    common_years <- intersect(colnames(df_patient), colnames(df_ppp))

    df_patient[, common_years] <- sweep(df_patient[, common_years], 2, as.numeric(df_ppp[, common_years]), `*`)
  }


  # If specified, filter based on category
  if(category != "all"){
    df_patient <- df_patient |>
      dplyr::filter(Category %in% category)
  }

  if(unit != "all"){
    df_patient <- df_patient |>
      dplyr::filter(Unit %in% unit)
  }

  return(df_patient)
}
