% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize.R
\name{map_hydro_data}
\alias{map_hydro_data}
\title{Map data used in composite hydrographs}
\usage{
map_hydro_data(x, num_years, parameter_cd = "72019")
}
\arguments{
\item{x}{aquifer data frame. Requires at least 3 columns. Two are required "site_no", "year",
and "value".}

\item{num_years}{integer number of years required}

\item{parameter_cd}{character, 5-digit parameter code, default is "72019".}
}
\value{
leaflet object
}
\description{
Map data used in composite hydrographs
}
\examples{
aquifer_data <- aquifer_data
num_years <- 30

map_data <- map_hydro_data(aquifer_data, num_years)
map_data
}
