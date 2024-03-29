% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze.R
\name{site_data_summary}
\alias{site_data_summary}
\title{site_data_summary}
\usage{
site_data_summary(x, value_col = "value", site_col = "site_no")
}
\arguments{
\item{x}{data frame}

\item{value_col}{name of value column. The default is \code{"value"}.}

\item{site_col}{name of site column. This is the column we are grouping by.}
}
\value{
data frame with 10 columns
}
\description{
Get summaries of data by site. Requires a column site_no, and will
take the summaries
}
\examples{
aquifer_data <- aquifer_data
aquifer_data <- aquifer_data[aquifer_data$parameter_cd == "72019", ]
summary_info <- site_data_summary(aquifer_data)
}
