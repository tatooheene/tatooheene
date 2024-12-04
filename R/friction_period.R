#' A function to download the friction period over one or multiple years
#' @param year The year of which the friction period should be downloaded, multiple years are possible. The default is the whole DataFrame
#' @param period The friction period that should be included (days/weeks),  default is including the whole dataframe
#' @param type If period is chosen, a decision can be made between the 5 year average and 1 year friction period in days/weeks. The default is the 5 year average
#' @keywords Generic, CBS
#' @export friction_period


friction_period <- function(
    year = "all",
    period = "all",
    type = "5_year_avg"){

  # Input validation with assertthat
  assertthat::assert_that(is.numeric(v_replace_val), msg = "`v_replace_val` must be numeric")

  # Filter the data for the specified years

  friction_period <- readRDS("data/df_fp_2023.rds")

  if(year != "all"){
    friction_period <- friction_period |>
      dplyr::filter(Year %in% year)}

  # Select the specified columns or all columns if none specified

  if(period == "weeks" & type == "5_year_avg"){
    friction_period <- friction_period |>
      dplyr::select(`Friction period weeks average over 5 years`) |>
      dplyr::pull(`Friction period weeks average over 5 years`)

    attr(friction_period, "label") <- NULL
    attr(friction_period, "unit") <- NULL

  }else if(period == "weeks" & type != "5_year_avg"){
    friction_period <- friction_period |>
      dplyr::select(`Friction period in weeks`) |>
      dplyr::pull(`Friction period in weeks`)

    attr(friction_period, "label") <- NULL
    attr(friction_period, "unit") <- NULL


  }else if(period == "days" & type == "5_year_avg"){
    friction_period <- friction_period |>
      dplyr::select(`Friction period days average over 5 years`) |>
      dplyr::pull(`Friction period days average over 5 years`)

    attr(friction_period, "label") <- NULL
    attr(friction_period, "unit") <- NULL

  }else if(period == "days" & type != "5_year_avg"){
    friction_period <- friction_period |>
      dplyr::select(`Friction period in days`) |>
      dplyr::pull(`Friction period in days`)

    attr(friction_period, "label") <- NULL
    attr(friction_period, "unit") <- NULL
  }

  return(friction_period)
}
