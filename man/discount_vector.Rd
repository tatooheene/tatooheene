% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discount_vector.R
\name{discount_vector}
\alias{discount_vector}
\title{A function to create a discount vector for a given time period}
\usage{
discount_vector(
  discount_rate = 0.03,
  start_time = 0,
  end_time,
  time_unit = c("years", "months", "weeks", "days"),
  discount_year_one = FALSE
)
}
\arguments{
\item{discount_rate}{The discount rate to use for the calculation. Default is 0.03. The guideline stipulates 0.03 for costs and 0.015 for effects.}

\item{start_time}{The start time for the discount vector. Default is 0.}

\item{end_time}{The end time for the discount vector.}

\item{time_unit}{The unit of time to use for the calculation. Default is "years", but "months", "weeks", and "days" are also valid options.}

\item{discount_year_one}{Logical value indicating whether to discount the first year as well. Default is FALSE.}
}
\value{
A numeric vector of discounted values for each time period.
}
\description{
This function calculates a discount vector for a given time period based on a discount rate.
}
\examples{
discount_vector(discount_rate = 0.015, start_time = 0, end_time = 5, time_unit = "years", discount_year_one = FALSE)
}
