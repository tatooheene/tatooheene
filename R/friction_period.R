#' Friction period lookup (days/weeks; 1-year or 5-year average)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Return friction periods from the internal CBS-based table.
#'
#' @param year Integer vector of years to return. If `NULL`, returns all years.
#' @param units One or more of `c("days","weeks")`. Default: `"weeks"`.
#' @param avg One or more of `c("1yr","5yr")`. Default: `"5yr"`.
#' @param output Either `"tibble"` (default) or `"value"`.
#'   If `"value"`, you must request exactly one year and one `(units, avg)` combo.
#' @param data Data source (mainly for testing); default is `tatooheene::df_fp`.
#'
#' @return A tibble when `output = "tibble"`, or a single numeric when `output = "value"`.
#'
#' @examples
#' # All years, 5-year average in weeks (default)
#' friction_period()
#'
#' # Specific year (2019), weeks 5-year average
#' friction_period(year = 2019)
#'
#' # Days (1-year) for multiple years
#' friction_period(year = 2018:2020, units = "days", avg = "1yr")
#'
#' # Single numeric value (requires one year + one combo)
#' friction_period(year = 2019, units = "weeks", avg = "5yr", output = "value")
#'
#' @export
#' @importFrom dplyr filter select all_of pull
#' @importFrom rlang .data
friction_period <- function(
    year   = NULL,
    units  = "weeks",
    avg    = "5yr",
    output = c("tibble", "value"),
    data   = tatooheene::df_fp
) {
  output <- match.arg(output, c("tibble", "value"))

  # Validate units/avg
  units <- match.arg(units, c("days", "weeks"), several.ok = TRUE)
  avg   <- match.arg(avg,   c("1yr", "5yr"),    several.ok = TRUE)

  # Column map
  col_map <- c(
    "days_1yr"  = "Friction period in days",
    "weeks_1yr" = "Friction period in weeks",
    "days_5yr"  = "Friction period days average over 5 years",
    "weeks_5yr" = "Friction period weeks average over 5 years"
  )

  # Build requested columns
  combos   <- as.vector(outer(units, avg, paste, sep = "_"))
  sel_cols <- unname(col_map[combos])

  # Validate years
  if (!is.null(year)) {
    if (!is.numeric(year) || any(is.na(year))) {
      stop("`year` must be numeric (e.g., 2019 or 2018:2020).", call. = FALSE)
    }
    yr_min <- min(data$Year, na.rm = TRUE)
    yr_max <- max(data$Year, na.rm = TRUE)
    if (any(year < yr_min | year > yr_max)) {
      stop(sprintf("Year out of range. Choose between %s and %s.", yr_min, yr_max), call. = FALSE)
    }
  }

  # Filter and select
  df <- data
  if (!is.null(year)) df <- dplyr::filter(df, Year %in% year)
  out <- dplyr::select(df, Year = Year, dplyr::all_of(sel_cols))

  if (output == "tibble") return(out)

  # output == "value": require exactly one year and one column
  if (nrow(out) != 1 || ncol(out) != 2) {
    stop('For `output = "value"`, provide exactly one `year` and a single `(units, avg)` combination.', call. = FALSE)
  }
  val <- dplyr::pull(out, 2)
  unname(as.numeric(val))
}
