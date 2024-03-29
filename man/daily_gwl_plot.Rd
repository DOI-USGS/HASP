% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frequency_analysis.R
\name{daily_gwl_plot}
\alias{daily_gwl_plot}
\title{Plot recent data}
\usage{
daily_gwl_plot(
  gw_level_dv,
  gwl_data,
  parameter_cd = NA,
  date_col = NA,
  value_col = NA,
  approved_col = NA,
  stat_cd = NA,
  start_date = NA,
  end_date = NA,
  historical_stat = "mean",
  month_breaks = FALSE,
  plot_title = "",
  subtitle = "U.S. Geological Survey",
  y_axis_label = "",
  flip = FALSE
)
}
\arguments{
\item{gw_level_dv}{data frame, daily groundwater level data. Often obtained
from \code{\link[dataRetrieval]{readNWISdv}}. Use \code{NULL} for no daily data.}

\item{gwl_data}{data frame returned from  \code{\link[dataRetrieval]{readNWISgwl}}, or
data frame with a date, value, and approval columns. Using the convention:
lev_dt (representing date), lev_age_cd (representing approval code), and lev_va
or sl_lev_va (representing value) will allow defaults to work.
Use \code{NULL} for no discrete data.}

\item{parameter_cd}{If data in gw_level_dv comes from NWIS, the parameter_cd
can be used to define the value_col. If the data doesn't come directly from
NWIS services, this can be set to \code{NA},and this argument will be ignored.}

\item{date_col}{the name of the date column. The default is \code{NA},
in which case, the code will try to get the column name automatically based on NWIS
naming conventions. If both gw_level_dv and gwl_data data frames
require custom column names, the first value of this input defines the date
column for gw_level_dv, and the second defines gwl_data.}

\item{value_col}{the name of the value column. The default is \code{NA},
in which case, the code will try to get the column name automatically based on NWIS
naming conventions. If both gw_level_dv and gwl_data data frames
require custom column names, the first value of this input defines the value
column for gw_level_dv, and the second defines gwl_data.}

\item{approved_col}{the name of the column to get provisional/approved status.
The default is \code{NA}, in which case, the code will try to get the column name
automatically based on NWIS naming conventions. If both gw_level_dv and
gwl_data data frames require custom column names, the first value of this
input defines the approval column for gw_level_dv, and the second defines gwl_data.#' @param stat_cd If data in gw_level_dv comes from NWIS, the stat_cd
can be used to help define the value_col.}

\item{stat_cd}{If data in gw_level_dv comes from NWIS, the stat_cd
can be used to help define the value_col.}

\item{start_date}{Date to start plot. If \code{NA} (which is the default),
the plot will start 2 years before the most recent value.}

\item{end_date}{Date to end plot. If \code{NA} (which is the default),
the plot will end with the latest measurement.}

\item{historical_stat}{the summary statistic to use for middle line of the plot. Either
"mean" or "median."}

\item{month_breaks}{a logical indicating whether to use monthly breaks for the plot}

\item{plot_title}{the title to use on the plot}

\item{subtitle}{character. Sub-title for plot, default is "U.S. Geological Survey".}

\item{y_axis_label}{the label to use for the y axis}

\item{flip}{logical. If \code{TRUE}, flips the y axis so that the smallest number is on top.
Default is \code{FALSE}.}
}
\value{
a ggplot object with a ribbon indicating the historical daily range,
the historical daily mean or median, and approved and provisional
daily data for the last two years
}
\description{
Calculates daily statistics based on all approved data.
Daily, discrete, or both types are included.
Historic median or mean are plotted based on all of the approved data.
}
\examples{

site <- "263819081585801"
p_code_dv <- "62610"
statCd <- "00001"
# gw_level_dv <- dataRetrieval::readNWISdv(site, p_code_dv, statCd = statCd)
gw_level_dv <- L2701_example_data$Daily

gwl_data <- L2701_example_data$Discrete

daily_gwl_plot(gw_level_dv,
               NULL,
               parameter_cd = "62610",
               plot_title = "Groundwater Level",
               historical_stat = "median")

daily_gwl_plot(gw_level_dv,
               gwl_data,
               parameter_cd = "62610",
               plot_title = "Groundwater Level",
               historical_stat = "median")

daily_gwl_plot(gw_level_dv,
               gwl_data,
               parameter_cd = "62610",
               plot_title = "Groundwater Level",
               month_breaks = TRUE,
               start_date = "2020-10-01",
               historical_stat = "median")

daily_gwl_plot(gw_level_dv, gwl_data,
               parameter_cd = "62610",
               plot_title = "Groundwater Level",
               month_breaks = TRUE,
               start_date = "2018-10-01",
               end_date = "2020-10-01",
               historical_stat = "median")

}
