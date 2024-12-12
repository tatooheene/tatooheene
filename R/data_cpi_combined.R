#' Consumer Price Index (CPI) data from CBS
#'
#' A subset data frame of Consumentenprijzen; prijsindex 2015=100 from CBS. Identifier: 83131NED
#'
#' @format ## `df_cpi_combined`
#' A data frame with 11 rows and 8 columns:
#' \describe{
#'   \item{Year from}{Year from in case of two consecutive years }
#'   \item{Year to}{Year ending in case of two consecutive years}
#'   \item{Percentage}{Percentage change in case of two consecutive years}
#'   \item{Factor}{Factor for multiplication in case of two consecutive years}
#'   \item{Year from'}{Year starting from, in the case of a range, the year up to the maximum year}
#'   \item{Year to'}{Year ending in case of a range, the year up to the maximum year}
#'   \item{Percentage'}{Percentage for multiplication in case of a range, the year up to the maximum year}
#'   \item{Factor'}{Factor for multiplication in case of a range, the year up to the maximum year}
#' }
#' @source <https://www.cbs.nl/nl-nl/cijfers>
"df_cpi_combined"
