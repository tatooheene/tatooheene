#' A function to download the friction period over one or multiple years
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' @param year The year of which the friction period should be downloaded, multiple years are possible. The default is the whole data frame
#' @param period The friction period that should be included (days/weeks),  default is including the whole dataframe
#' @param type If period is chosen, a decision can be made between the 5 year average and 1 year friction period in days/weeks. The default is the 5 year average
#' @keywords Generic, CBS
#' @return A data frame with friction periods for all years or a selection of years and variables.
#' @examples
#' # Example usage of the depreciation_interest function
#' friction_period(year = 2019, period = "weeks", type = "5_year_avg")
#' @export friction_period

friction_period <- function(
    year = "all",
    period = "all",
    type = "5_year_avg"){

  # Filter the data for the specified years

  df <- tatooheene::df_fp

  if (year == "all" & period == "all" & type == "5_year_avg") {
    friction_period_out <- df

  } else if(year != "all"){
    friction_period_out <- df |>
      dplyr::filter(Year %in% year)}else if(
        # Select the specified columns or all columns if none specified
        period == "weeks" & type == "5_year_avg"){
        friction_period_out <- df |>
          dplyr::pull(`Friction period weeks average over 5 years`)

        attr(friction_period_out, "label") <- NULL
        attr(friction_period_out, "unit") <- NULL

      }else if(period == "weeks" & type != "5_year_avg"){
        friction_period_out <- df |>
          dplyr::pull(`Friction period in weeks`)

        attr(friction_period_out, "label") <- NULL
        attr(friction_period_out, "unit") <- NULL

      }else if(period == "days" & type == "5_year_avg"){
        friction_period_out <- df |>
          dplyr::pull(`Friction period days average over 5 years`)

        attr(friction_period_out, "label") <- NULL
        attr(friction_period_out, "unit") <- NULL

      }else if(period == "days" & type != "5_year_avg"){
        friction_period_out <- df |>
          dplyr::pull(`Friction period in days`)

        attr(friction_period_out, "label") <- NULL
        attr(friction_period_out, "unit") <- NULL
      }

  friction_period_out
}
