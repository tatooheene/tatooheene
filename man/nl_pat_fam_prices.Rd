% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nl_pat_fam_prices.R
\name{nl_pat_fam_prices}
\alias{nl_pat_fam_prices}
\title{A function to download the Patient & Family Reference prices of the Dutch Costing Manual for one or multiple years}
\usage{
nl_pat_fam_prices(
  year = "all",
  category = "all",
  currency = c("EUR", "INT$"),
  unit = "all"
)
}
\arguments{
\item{year}{The year of which the reference price should be downloaded, multiple years are possible, default is the whole dataset (year = "all")}

\item{category}{The category of prices that should be included (one or more categories), default is including all categories}

\item{currency}{The currency of the output of the prices. A decision can be made between EUR and INT$, the default is EUR.}

\item{unit}{The reference price that should be included (one or multiple reference prices),  default is including the whole data frame}
}
\value{
A dataframe or value with the Patient & Family Reference price(s) of the Dutch Costing Manual for the specified years
}
\description{
This function downloads the Patient & Family Reference prices of the Dutch Costing Manual for one or multiple years. The prices are available in Euro (EUR) or International Dollar (INT$).
}
\examples{
Example usage of the nl_pat_fam_prices function
nl_pat_fam_prices(year = 2022, category = "Transportation", currency = "INT$", unit = "Car, cost per kilometer")
}
\keyword{&}
\keyword{Costing}
\keyword{Dutch}
\keyword{Family}
\keyword{Generic,}
\keyword{Manual,}
\keyword{Patient}
\keyword{Prices}
\keyword{Prices,}
\keyword{Reference}
