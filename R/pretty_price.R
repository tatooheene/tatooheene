#' A function to write pretty prices in `bookdown` reports
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This function writes pretty prices in `bookdown` reports. The function uses the `formatC()` function to format the number and adds the currency to the end of the number.
#' @param x A number to be printed
#' @param currency The name of the currency
#' @param digi Number of digits
#' @param ... Extra arguments for `formatC()`
#' @return A pretty price with the currency
#' @examples
#' # Example usage of the pretty_price function
#' pretty_price(1000, currency = "EUR")
#' @export pretty_price

pretty_price <- function(x, digi = 2, currency = "EUR", ...){

  paste(formatC(round(x, digits = digi),
                big.mark = ",",
                format = "f",
                digits = digi, ...), currency)

}
