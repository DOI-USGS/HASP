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
#' Example data representing Basin and Range basin-fill aquifers.
#'
#' @name aquifer_data
#' @rdname sampleData
#' @docType data
#' @keywords water quality data
#' @examples 
#' head(aquifer_data)
NULL

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

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste(strwrap('USGS Research Package:
https://owi.usgs.gov/R/packages.html#research
PACKAGE IN HEAVY DEVELOPMENT
NO FUTURE BEHAVIOR PROMISED', width = 40),
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