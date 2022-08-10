#' Seasonal Kendall Trend Test
#' 
#' Test for five and twenty year trends in groundwater level using the 
#' Seasonal Kendall Trend Test.
#' 
#' At least 10 readings per year for the last 5 years are required for the
#' 5-year test, and at least 6 readings for the last 20 years are required
#' for the 20-year test. The current calendar year is excluded by default.
#' 
#' @param gw_level_dv daily groundwater level data frame. Often obtained from from \code{readNWISdv}
#' @param gwl_data data frame returned from dataRetrieval::readNWISgwl, or 
#' data frame with mandatory columns lev_dt (representing date), lev_age_cd (representing
#' approval code), and a column representing the measured value (either lev_va,
#' sl_lev_va, or value).
#' @param date_col the heading of the date column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param value_col name of value column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param approved_col name of column to get provisional/approved status.
#' @param stat_cd If data in gw_level_dv comes from NWIS, the stat_cd 
#' can be used to help define the value_col.
#' @param seasonal logical. Use a seasonal kendall test or not seasonal. Default is \code{TRUE}.
#' @param enough_5 number per year. Default is 10.
#' @param enough_20 number per year. Default is 6.
#' @param alpha the confidence level to use for statistical significance
#' @param include_current_year a logical indicating whether to include data from
#' the current calendar year in the test.
#' @param parameter_cd If data in gw_level_dv comes from NWIS, the parameter_cd 
#' can be used to define the value_col.
#'  If the data doesn't come directly from NWIS services, this 
#' can be set to \code{NA},and this argument will be ignored.
#' @importFrom stats as.formula
#' @return a data frame of test results from 5 and 20 year Kendall Seasonal Trend test
#' 
#' @export
#' 
#' @examples 
#' 
#' # site <- "260041080093102"
#' # gw_level_data <- dataRetrieval::readNWISgwl(site)
#' 
#' # Using package example data:
#' gwl_data <- L2701_example_data$Discrete
#' kendall_test_5_20_years(NULL,
#'                         gwl_data,
#'                         parameter_cd = "62610")
#'                         
#' gw_level_dv <- L2701_example_data$Daily
#' kendall_test_5_20_years(gw_level_dv,
#'                         gwl_data,
#'                         parameter_cd = "62610")
kendall_test_5_20_years <- function(gw_level_dv, 
                                    gwl_data,
                                    parameter_cd = NA,
                                    date_col = NA,
                                    value_col = NA,
                                    approved_col = NA,
                                    stat_cd = NA,
                                    alpha = 0.95,
                                    seasonal = TRUE, 
                                    enough_5 = 10, enough_20 = 6,
                                    include_current_year = FALSE) {

  data_list <- set_up_data(gw_level_dv = gw_level_dv, 
                           gwl_data = gwl_data, 
                           parameter_cd = parameter_cd,
                           date_col = date_col,
                           value_col = value_col, 
                           approved_col = approved_col,
                           stat_cd = stat_cd)
  
  gw_level_dv <- data_list$gw_level_dv
  gwl_data <- data_list$gwl_data

  gwl <- dplyr::bind_rows(gw_level_dv,
                          gwl_data)
  
  gwl$year <- as.numeric(format(gwl$Date, "%Y"))
  
  if(!include_current_year) {
    current_year <- max(gwl$year, na.rm = TRUE)
    gwl <- gwl[gwl$year != current_year, ]
  }

  latest_measured_year <- max(gwl$year, na.rm = TRUE)
  
  enough_data_5yr <- enough_data(gwl, date_col = "Date", 
                                 required_per_year = enough_5) 
  enough_data_20yr <- enough_data(gwl, date_col = "Date", 
                                  n_years = 20, 
                                  required_per_year = enough_20) # 50% of monthly data

  if(seasonal){
    gwl$month <- as.numeric(format(gwl$Date, "%m"))
    form <- as.formula(Value ~ month + year)
  } else {
    gwl$Date <- as.numeric(as.Date(gwl$Date))
    form <- as.formula(Value ~ Date )
  }
  
  test <- vector()
  tau <- vector()
  pValue <- vector()
  slope <- vector()
  intercept <- vector()
  trend <- vector()
  
  if(enough_data_5yr) {
    # Don't assume the tail is bringing back all years:
    last_5 <- dplyr::filter(gwl, year >= latest_measured_year - 5)
    
    if(seasonal){
      test_5yr <- 
        EnvStats::kendallSeasonalTrendTest(form, 
                                           data = last_5)      
    } else {
      test_5yr <- EnvStats::kendallTrendTest(form, 
                                             data = last_5)
    }

    test[length(test) + 1] <- "5-year trend"
    tau[length(tau) + 1] <- test_5yr$estimate['tau']
    pValue[length(pValue) + 1] <- test_5yr$p.value[ifelse(seasonal, 'z (Trend)', 'z')]
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
    last_20 <- dplyr::filter(gwl, year >= latest_measured_year - 20)
    
    if(seasonal){
      test_20yr <- 
        EnvStats::kendallSeasonalTrendTest(form, 
                                           data = last_20)      
    } else {
      test_20yr <- EnvStats::kendallTrendTest(form, 
                                             data = last_20)
    }

    test[length(test) + 1] <- "20-year trend"
    tau[length(tau) + 1] <- test_20yr$estimate['tau']
    pValue[length(pValue) + 1] <- test_20yr$p.value[ifelse(seasonal, 'z (Trend)', 'z')]
    slope[length(slope) + 1] <- test_20yr$estimate['slope']
    intercept[length(intercept) + 1] <- test_20yr$estimate['intercept']
  } else {
    test[length(test) + 1] <- "20-year trend"
    tau[length(tau) + 1] <- NA
    pValue[length(pValue) + 1] <- NA
    slope[length(slope) + 1] <- NA
    intercept[length(intercept) + 1] <- NA
  }
  
  test_results <- data.frame(test, tau, pValue, slope, intercept, stringsAsFactors = FALSE)
  test_results <- dplyr::mutate(test_results,
                                trend = ifelse(pValue < (1 - alpha),
                                               ifelse(slope > 0,
                                                      "Up",
                                                      "Down"),
                                               "Not significant"))
  return(test_results)
  
}

#' Monthly mean
#' 
#' Take the mean of each month, filter if there's not at least 15 days, 
#' and create a new date that is the midpoint of each month.
#' 
#' @param x data.frame
#' @param date_col character name of date column
#' @param value_col character name of value column
#' @export
#' @examples 
#' 
#' # site <- "263819081585801"
#' parameterCd <- "62610"
#' # statCd <- "00001"
#' # gw_level_dv <- dataRetrieval::readNWISdv(site, parameterCd, 
#' #                                           statCd = statCd)
#' # Using package example data:
#' gw_level_dv <- L2701_example_data$Daily
#' 
#' site_statistics <- monthly_frequency_table(gw_level_dv, 
#'                                            NULL,
#'                                            parameter_cd = parameterCd)
#' 
#' gw_monthly <- monthly_mean(gw_level_dv)
#' 
monthly_mean <- function(x,
                         date_col = "Date",
                         value_col = "X_62610_00001"){
  
  days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  x$result <- x[[value_col]]
  x$date <- x[[date_col]]
  
  monthly_mean <- x %>% 
    dplyr::mutate(month = as.numeric(format(date, "%m")),
           year = as.numeric(format(date, "%Y"))) %>% 
    dplyr::group_by(year, month) %>% 
    dplyr::summarize(mean_va = mean(result, na.rm = TRUE),
              n_days = dplyr::n()) %>% 
    dplyr::filter(n_days > 14) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(mid_date = as.Date(paste(year, month, 15, sep = "-")))
    
  return(monthly_mean)
  
}

enough_data <- function(x, 
                        date_col = "lev_dt",
                        n_years = 5, 
                        required_per_year = 10){

  x$year <- as.numeric(format(x[[date_col]], "%Y")) 

  latest_measured_year <- max(x$year, na.rm = TRUE)
  
  x <- dplyr::filter(x, year >= latest_measured_year - n_years)
  
  yearly_count <- dplyr::count(x, year)
 
  if(nrow(yearly_count) < n_years) {
    message("Total data time span is less than ", n_years," years")
    enough_data <- FALSE
  } else {

    if(!all(yearly_count$n >= required_per_year)) {
      message("Not enough measurements in each of the the last ", n_years,
              " years to proceed")
      enough_data <- FALSE
    } else {
      enough_data <- TRUE
    }
  }
  return(enough_data)
}