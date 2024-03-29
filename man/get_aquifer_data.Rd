% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_data.R
\name{get_aquifer_data}
\alias{get_aquifer_data}
\title{get_aquifer_data}
\usage{
get_aquifer_data(aquiferCd, startDate, endDate, parameter_cd = "72019")
}
\arguments{
\item{aquiferCd}{character. To see valid aquifer codes, see the included data
frame \code{local_aqfr}.}

\item{startDate}{date or string. Beginning date of when to pull data.}

\item{endDate}{date of string  Ending date to pull data.}

\item{parameter_cd}{5-digit character USGS parameter code.}
}
\description{
Get USGS data based on aquiferCd
}
\examples{
end_date <- "2021-01-01"
start_date <- "1989-12-31"

aquiferCd <- "S100CSLLWD"
\donttest{
aq_data <- get_aquifer_data(aquiferCd, start_date, end_date)
}
}
