% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discount_stream.R
\name{discount_stream}
\alias{discount_stream}
\title{Calculate the discounted sum of a stream of costs or effects}
\usage{
discount_stream(
  values,
  discount_rate = c("costs", "effects"),
  discount_year_one = FALSE
)
}
\arguments{
\item{values}{A numeric vector of costs or effects over time (one value per period).}

\item{discount_rate}{A numeric discount rate (e.g. 0.03 for 3\%). Default is 0.03.}

\item{discount_year_one}{Logical: should the first year (t = 0) be discounted? Default is FALSE.}
}
\value{
A numeric value: the discounted values.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
This function calculates the net present value of a vector of future costs or effects using constant discounting, as described in the Dutch guideline for economic evaluations in healthcare.
}
\examples{
# Constant cost of 100 for 3 years, no discounting in year 0
discount_stream(values = rep(100, 3), discount_rate = 0.03, discount_year_one = FALSE)

}
