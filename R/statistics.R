#' Seasonal Kendall Trend Test
#' 
#' Test for five and twenty year trends in groundwater level using the 
#' Seasonal Kendall Trend Test.
#' 
#' At least 10 readings per year for the last 5 years are required for the
#' 5-year test, and at least 6 readings for the last 20 years are required
#' for the 20-year test. The current calendar year is excluded by default.
#' 
#' @param gwl data frame that must include a numeric column defined by "value_col",
#'  and a Date or POSIXct column defined by "date_col"
#' @param date_col name of date column.
#' @param value_col name of value column.
#' @param seasonal logical. Use a seasonal kendall test or not seasonal. Default is \code{TRUE}.
#' @param enough_5 number per year. Default is 10.
#' @param enough_20 numbr per year. Default is 6.
#' @param alpha the confidence level to use for statistical significance
#' @param include_current_year a logical indicating whether to include data from
#' the current calendar year in the test.
#' @param parameter_cd_gwl Parameter code to be filtered to in a column specifically
#' named "parameter_cd". If the data doesn't come directly from NWIS services, this 
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
#' kendell_test_5_20_years(gwl_data, parameter_cd_gwl = "62610")
#' 
kendell_test_5_20_years <- function(gwl, 
                                    parameter_cd_gwl = NA,
                                    date_col = "lev_dt",
                                    value_col = "sl_lev_va",
                                    alpha = 0.95,
                                    seasonal = TRUE, 
                                    enough_5 = 10, enough_20 = 6,
                                    include_current_year = FALSE) {
  
  year <- month <- ".dplyr"
  
  if(!all(c(date_col, value_col) %in% names(gwl))) {
    stop("gwl should include ", date_col, " and ", value_col, " columns")
  }
  
  gwl <- filter_pcode(gwl, parameter_cd_gwl)
  
  gwl$year <- as.numeric(format(gwl[[date_col]], "%Y"))
  
  if(!include_current_year) {
    current_year <- max(gwl$year, na.rm = TRUE)
    gwl <- gwl[gwl$year != current_year, ]
  }

  latest_measured_year <- max(gwl$year, na.rm = TRUE)
  
  
  enough_data_5yr <- enough_data(gwl, date_col = date_col, 
                                 required_per_year = enough_5) 
  enough_data_20yr <- enough_data(gwl, date_col = date_col, 
                                  n_years = 20, 
                                  required_per_year = enough_20) # 50% of monthly data

  if(seasonal){
    gwl$month <- as.numeric(format(gwl[[date_col]], "%m"))
    form <- as.formula(paste(value_col, " ~ month + year"))
  } else {
    gwl[date_col] <- as.numeric(gwl[[date_col]])
    form <- as.formula(paste(value_col, " ~ ", date_col ))
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
#' gw_monthly <- monthly_mean(gw_level_dv)
#' 
#' kendell_test_5_20_years(gw_monthly, seasonal = TRUE, 
#'                         date_col = "mid_date", value_col = "mean_va")
#' 
monthly_mean <- function(x,
                         date_col = "Date",
                         value_col = "X_62610_00001"){
  
  year <- month <- n_days <- mean_va <- ".dplyr"
  
  days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  monthly_mean <- x %>% 
    mutate(month = as.numeric(format(!!sym(date_col), "%m")),
           year = as.numeric(format(!!sym(date_col), "%Y"))) %>% 
    group_by(year, month) %>% 
    summarize(mean_va = mean(!!sym(value_col), na.rm = TRUE),
              n_days = n()) %>% 
    filter(n_days > 14) %>% 
    ungroup() %>% 
    mutate(mid_date = as.Date(paste(year, month, 15, sep = "-")))
    
  return(monthly_mean)
  
}

enough_data <- function(x, 
                        date_col = "lev_dt",
                        n_years = 5, 
                        required_per_year = 10){
  
  year <- ".dplyr"
  
  x$year <- as.numeric(format(x[[date_col]], "%Y")) 

  latest_measured_year <- max(x$year, na.rm = TRUE)
  
  x <- dplyr::filter(x, year >= latest_measured_year - n_years)
  
  yearly_count <- count(x, year)
 
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