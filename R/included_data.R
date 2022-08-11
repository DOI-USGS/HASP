#' Aquifer data included
#' 
#' National aquifer data
#'
#' @aliases nat_aqfr_state
#' @name nat_aqfr_state
#' @return data frame with columns Country, State, nat_aqfr_cd, long_name, and other_cd
#' @docType data
#' @rdname included_data
#' @keywords datasets
#' @export nat_aqfr_state
#' @examples
#' head(nat_aqfr_state)
NULL

#' @aliases summary_aquifers
#' @name summary_aquifers
#' @return data frame with columns long_name, nat_aqfr_cd, state_indexes, and states
#' @docType data
#' @rdname included_data
#' @keywords datasets
#' @export summary_aquifers
#' @examples
#' head(summary_aquifers)
NULL

#' Example aquifer data
#'
#' Example data representing Basin and Range basin-fill aquifers (N100BSNRGB).
#'
#' @name aquifer_data
#' @rdname sampleData
#' @docType data
#' @keywords water quality data
#' @examples 
#' head(aquifer_data)
NULL
# Example to get the data:
# start_date <- "1988-10-01"
# end_date <- "2021-01-01"
# aquiferCd <- "N100BSNRGB"
# aquifer_data <- get_aquifer_data(aquiferCd, start_date, end_date)
# aquifer_data <- dplyr::filter(aquifer_data, parameter_cd %in%
#                                 c( "72019"))
# save(aquifer_data, file = "data/aquifer_data.RData", compress = "xz")


#' Local Aquifer Code Listing
#' 
#' Local aquifer data codes
#'
#' @aliases local_aqfr
#' @name local_aqfr
#' @return data frame with aqfr_cd, Aqfr_Name_prpr
#' @docType data
#' @rdname included_data
#' @keywords datasets
#' @export local_aqfr
#' @examples
#' head(local_aqfr)
NULL

#' HASP data included
#' 
#' markerTable raw data
#'
#' @aliases markerTable
#' @name markerTable
#' @return data frame with columns Trend and MarkerDescription
#' @docType data
#' @rdname included_data
#' @keywords datasets
NULL

#' @aliases markerTable2
#' @name markerTable2
#' @return data frame with columns trendType, trend, markerDescription, r_lwd
#' @docType data
#' @rdname included_data
#' @keywords datasets
NULL

#' Example groundwater level data
#' 
#' Example data from site 263819081585801 L-2701. Data is a named list of
#' "Daily" for daily groundwater level at the site and "Discrete" for discrete
#' groundwater level measurements from the site. Updated April 9, 2021.
#' 
#' @name L2701_example_data
#' @rdname example_data
#' @docType data
#' @examples 
#' head(L2701_example_data$Daily)
#' head(L2701_example_data$Discrete)
#' head(L2701_example_data$QW)
NULL

# library(dataRetrieval)
# site <- "263819081585801"
# parameterCd <- c("00095","90095","00940","99220")
# qw_data <- dataRetrieval::readWQPqw(paste0("USGS-", site),
#                                     parameterCd)
# gwl_data <- dataRetrieval::readNWISgwl(site)
# parameterCd <- "62610"
# statCd <- "00001"
# dv <- dataRetrieval::readNWISdv(site,
#                                 parameterCd,
#                                 statCd = statCd)
# 
# L2701_example_data <- list("Daily" = dv,
#                            "Discrete" = gwl_data,
#                            "QW" = qw_data)
# save(L2701_example_data, file = "data/L2701_example_data.RData")

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste(strwrap('More information:
https://rconnect.usgs.gov/HASP_docs/', width = 40),
          collapse='\n'))
}


#' Analyze groundwater data
#' 
#' \code{HASP} includes a set of functions to get groundwater data
#' as grouped by aquifers and create composite hydrographs.
#' 
#' 
#' \tabular{ll}{
#' Package: \tab HASP\cr
#' Type: \tab Package\cr
#' License: \tab Unlimited for this package, dependencies have more restrictive licensing.\cr
#' Copyright: \tab This software is in the public domain because it contains materials
#' that originally came from the United States Geological Survey, an agency of
#' the United States Department of Interior. For more information, see the
#' official USGS copyright policy at
#' https://www.usgs.gov/visual-id/credit_usgs.html#copyright\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#'
#' @name HASP-package
#' @docType package
#' @keywords Groundwater levels
NULL

utils::globalVariables(c("lev_dt", "nYears","minMed", "maxMed", "name", "value",
                         "group", "plot_month_med", "p50", "sl_lev_va", "plot_month_last",
                         "ymax", "ymin", "year", "month", "result", "n_days", "site_no",
                         "p5", "p10", "p25", "p75", "p90", "p95", "trend", "med_site",
                         "mean_med", "max_med", "min_med", "x_norm", "station_nm",
                         "prep_map_data", "MonitoringLocationIdentifier", "site", "val",
                         "ResultMeasureValue", "lev_va", "agency_cd", "tz_cd", "state_call", 
                         "pcode", "lev_status_cd", "ActivityStartDate", "cd", "week_start",
                         "..eq.label..", "..rr.label..", "ActivityStartDateTime", "qualifier",
                         "Approve", "Aqfr_Name_prpr", "CharacteristicName", "Chloride", "DOY", "Data Type", "Date", "J",
                         "Specific conductance", "Value", "begin", "begin_date", "condition", "count", "count_nu",
                         "county_cd", "data_type_cd", "dateTime", "dec_lat_va", "dec_long_va", "end", "end_date",
                         "gw_code", "gw_level", "gw_level_cd", "is_na_after", "is_na_before", "is_point", "lat_va",
                         "lev_age_cd", "long_va", "median", "middle", "n_sites_year", "parm_cd", "plot_week_last",
                         "plot_week_med", "results", "state_cd", "week", "x", "x1", "x2", "y", "y1", "y2"))

