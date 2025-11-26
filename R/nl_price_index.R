#' A function to calculate the Consumer Price Index (CPI) for a given year range.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function provides the Consumer Price Index (CPI) for a given year range both in a factor or dataframe based on CBS data and further described in 2.6.1.1 of the Dutch EE guideline
#'
#' @param start_year start year for CPI output table or factor
#' @param end_year End year for CPI output table or factor
#' @param output Which output we would like to see. "factor": is the factor from start to end year, "table" is the table of all CPIs from start to end year
#' @return Dataframe or factor with CPI data from start year to end year
#' @keywords CPI, Consumer Price Index, CBS, Dutch EE guideline
#' @examples
#' # Example usage of the nl_price_index function
#' # Get the CPI factor from 2013 to 2023
#' nl_price_index(start_year = 2013, end_year = 2023, output = "factor")
#'
#' # Get the CPI table from 2013 to 2023
#' nl_price_index(start_year = 2013, end_year = 2023, output = "table")
#'
#' @export nl_price_index

nl_price_index <- function(start_year = 2013,
                  end_year = 2023,
                  output = c("table", "factor")){


  # Read in the dataset
  df <- tatooheene::df_cpi_combined


  # Output
  output <- match.arg(output)

  # Input validation with assertthat
  assertthat::assert_that(is.numeric(start_year), msg = "`start_year` must be numeric")
  assertthat::assert_that(is.numeric(end_year),   msg = "`end_year` must be numeric")
  assertthat::assert_that(start_year < end_year,  msg = "`start_year` must be smaller than `end_year`")
  assertthat::assert_that(output %in% c("table", "factor"),
                          msg = "`output` must be one of 'table' or 'factor'")

  #assertthat::assert_that(start_year >= min(df$`Year to`),
                         # msg = paste0("`start_year` must be greater than or equal to the minimum year in the dataset which is:", lowest_year_from, sep = " "))

#  assertthat::assert_that(end_year <= max(df$`Year to`),
                    #      msg = "`end_year` must be less than or equal to the maximum year in the dataset")


  msg_year_from <- assertthat::validate_that(
    start_year >= min(df$`Year from`),
    msg = paste("`start_year` must be greater than or equal to the minimum year in the dataset which is:", min(df$`Year from`), sep = " ")
  )

  if (!isTRUE(msg_year_from)) {
    message(msg_year_from)
  }

  msg_year_to <- assertthat::validate_that(
    end_year <= max(df$`Year to`),
    msg = paste("`end_year` must be less than or equal to the maximum year in the dataset which is:", max(df$`Year to`), sep = " ")
  )

  if (!isTRUE(msg_year_to)) {
    message(msg_year_to)
  }




  # Check if the output is a table or factor

  # If the output is a factor:
  if(output == "factor"){
    factor <- df |>
      dplyr::filter(`Year from'` == start_year,
                    `Year to'`   == end_year) |>
      dplyr::pull(`Factor'`)

      return(factor)

    # If the output is a table:
  } else if(output == "table"){
    table <- df |>
      dplyr::filter(`Year to` >= (start_year + 1) & `Year to` <= end_year)

    return(table)
  }

}

