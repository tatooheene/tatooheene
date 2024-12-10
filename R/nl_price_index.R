#' A function to calculate the Consumer Price Index (CPI) for a given year range.
#'
#' This function provides the Consumer Price Index (CPI) for a given year range both in a factor or dataframe
#'
#' @param start_year start year for CPI output table
#' @param end_year End year for CPI output table
#' @param output Which output we would like to see. "factor": is the factor from start to end year, "table" is the table of all CPIs from start to end year
#' @return dataframe with CPI data (CPI, percentage and multiplication factor) from start year to end year
#' @examples
#' example usage of the nl_price_index function
#' nl_price_index(2015, 2019, output = "table")
#'
nl_price_index <- function(start_year = 2013,
                  end_year = 2023,
                  output = "table"){

  df <- df_cpi_combined

  # Check if the output is a table or factor
  # If the output is a factor:
  if(output == "factor"){
    factor <- df |>
      dplyr::filter(`Year to` >= (start_year+1) & `Year to` <= end_year) |>
      dplyr:: summarise(cumulative = prod(Percentage)) |>
      dplyr::pull(cumulative)

      return(factor)

    # If the output is a table:
  }else if(output == "table"){
    table <- df |>
      dplyr::filter(`Year to` >= start_year & `Year to` <= end_year)

    return(table)
  }

}

