#' Seasonal Kendall Trend Test
#' 
#' Test for five and twenty year trends in groundwater level using the 
#' Seasonal Kendall Trend Test.
#' 
#' At least 10 readings per year for the last 5 years are required for the
#' 5-year test, and at least 6 readings for the last 20 years are required
#' for the 20-year test. The current calendar year is excluded by default.
#' 
#' @param gw_level_data groundwater level data from the \code{readNWISgwl} from
#' the dataRetrieval package
#' @param alpha the confidence level to use for statistical significance
#' @param include_current_year a logical indicating whether to include data from
#' the current calendar year in the test.
#' 
#' @return a data frame of test results from 5 and 20 year Kendall Seasonal Trend test
#' 
#' @export
#' @importFrom utils tail
#' 
#' @examples 
#' 
#' site <- "260041080093102"
#' gw_level_data <- dataRetrieval::readNWISgwl(site)
#' seasonal_kendall_trend_test(gw_level_data)
#' 

seasonal_kendall_trend_test <- function(gw_level_data, alpha = 0.95,
                                        include_current_year = FALSE) {
  
  year <- lev_dt <- month <- sl_lev_va <- ".dplyr"
  
  if(!all(c("lev_dt", "sl_lev_va") %in% names(gw_level_data))) {
    stop("gw_level_data should include 'sl_lev_va' and 'lev_dt' columns")
  }
  
  gw_level_data <- dplyr::mutate(gw_level_data, 
                               year = lubridate::year(lev_dt),
                               month = lubridate::month(lev_dt))
  current_year <- lubridate::year(Sys.Date())
  if(!include_current_year) {
    gw_level_data <- gw_level_data[gw_level_data$year != current_year, ]
  }
  yearly_count <- dplyr::group_by(gw_level_data, year)
  yearly_count <- dplyr::summarise(yearly_count, n = dplyr::n())
  
  # Need at least 80% complete data for the last 5 years to procede with the 5 year test
  if(nrow(yearly_count) < 5) {
    message("Data time span is less than 5 years")
    enough_data_5yr <- FALSE
  } else {
    if(!all(tail(yearly_count$n, 5) >= 10)) {
      message("Not enough measurements in each of the the last 5 years to proceed with 5 year test")
      enough_data_5yr <- FALSE
    } else {
      enough_data_5yr <- TRUE
    }
  }
  
  # Need at least 50% complete data for the last 20 years to procede with the 20 year test
  if(nrow(yearly_count) < 20) {
    message("data time span is less than 20 years")
    enough_data_20yr <- FALSE
  } else {
    if(!all(tail(yearly_count$n, 20) >= 6)) {
      message("Not enough measurements in each of the last 20 years to proceed with the 20 year test")
      enough_data_20yr <- FALSE
    } else {
      enough_data_20yr <- TRUE
    }
  }
  
  test <- vector()
  tau <- vector()
  pValue <- vector()
  slope <- vector()
  intercept <- vector()
  trend <- vector()
  
  if(enough_data_5yr) {
    last_5 <- dplyr::filter(gw_level_data, year %in% tail(yearly_count$year, 5))
    test_5yr <- 
      EnvStats::kendallSeasonalTrendTest(sl_lev_va ~ month + year, 
                                         data = last_5)
    test[length(test) + 1] <- "5-year trend"
    tau[length(tau) + 1] <- test_5yr$estimate['tau']
    pValue[length(pValue) + 1] <- test_5yr$p.value['z (Trend)']
    slope[length(slope) + 1] <- test_5yr$estimate['slope']
    intercept[length(intercept) + 1] <- test_5yr$estimate['intercept']
  } else {
    test[length(test) + 1] <- "5-year trend"
    tau[length(tau) + 1] <- NA
    pValue[length(pValue) + 1] <- NA
    slope[length(slope) + 1] <- NA
    intercept[length(intercept) + 1] <- NA
  }
  
  if(enough_data_20yr) {
    last_20 <- dplyr::filter(gw_level_data, year %in% tail(yearly_count$year, 20))
    test_20yr <- 
      EnvStats::kendallSeasonalTrendTest(sl_lev_va ~ month + year, 
                                         data = last_20)
    test[length(test) + 1] <- "20-year trend"
    tau[length(tau) + 1] <- test_20yr$estimate['tau']
    pValue[length(pValue) + 1] <- test_20yr$p.value['z (Trend)']
    slope[length(slope) + 1] <- test_20yr$estimate['slope']
    intercept[length(intercept) + 1] <- test_20yr$estimate['intercept']
  } else {
    test[length(test) + 1] <- "20-year trend"
    tau[length(tau) + 1] <- NA
    pValue[length(pValue) + 1] <- NA
    slope[length(slope) + 1] <- NA
    intercept[length(intercept) + 1] <- NA
  }
  
  test_results <- data.frame(test, tau, pValue, slope, intercept)
  test_results <- dplyr::mutate(test_results,
                                trend = ifelse(pValue < (1 - alpha),
                                               ifelse(slope > 0,
                                                      "Up",
                                                      "Down"),
                                               "Not significant"))
  return(test_results)
  
}