#' Trend Test
#' 
#' Test for five and twenty year trends in groundwater level using the 
#' Regional Kendall Test.
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
#' @param days_required_per_month integer. Number of days required per month. 
#' Default is 14.
#' @param pctComplete number percentage complete.
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
#' # site <- "263819081585801"
#' # gw_level_data <- dataRetrieval::readNWISgwl(site)
#' 
#' # Using package example data:
#' gwl_data <- L2701_example_data$Discrete
#' trend_test(NULL,
#'                         gwl_data,
#'                         parameter_cd = "62610")
#'                         
#' gw_level_dv <- L2701_example_data$Daily
#' trend_test(gw_level_dv,
#'            gwl_data,
#'            parameter_cd = "62610")
#'            
#' trend_test(gw_level_dv,
#'            gwl_data,
#'            parameter_cd = "62610",
#'            n_years = 5)
#'            
#' trend_test(gw_level_dv,
#'            gwl_data,
#'            parameter_cd = "62610",
#'            n_years = 5,
#'            days_required_per_month = 0)
#'            
trend_test <- function(gw_level_dv, 
                       gwl_data,
                       n_years = 10,
                       parameter_cd = NA,
                       date_col = NA,
                       value_col = NA,
                       approved_col = NA,
                       stat_cd = NA,
                       alpha = 0.95,
                       pctComplete = 0.5,
                       days_required_per_month = 14,
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
                          gwl_data) %>%
    dplyr::mutate(year = as.numeric(format(Date, "%Y")),
                  month = as.numeric(format(Date, "%m")),
                  doy = as.numeric(format(Date, "%j"))) %>%
    dplyr::group_by(year, month) %>%
    dplyr::summarize(monthlyMean = mean(Value, na.rm = TRUE),
                     ndays = length(doy)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(midMonth = as.Date(sprintf("%s-%s-15", year, month)),
                  decYear = decimalDate(midMonth),
                  gap = decYear - dplyr::lag(decYear) >= 1000)  %>%
    dplyr::filter(ndays > !!days_required_per_month)
  

  if(!include_current_year) {
    current_year <- max(gwl$year, na.rm = TRUE)
    gwl <- gwl[gwl$year != current_year, ]
  }

  latest_measured_year <- max(gwl$year, na.rm = TRUE)


  enough_data_n <- enough_data(gwl,
                               pctComplete = pctComplete,
                               n_years = n_years) 
  enough_data_por <- enough_data(gwl,
                                 n_years = 11,
                                 pctComplete = pctComplete,
                                 por = TRUE) # 50% of monthly data

  test <- vector()
  tau <- vector()
  pValue <- vector()
  slope <- vector()
  intercept <- vector()
  trend <- vector()
  
  if(enough_data_n) {
    test[length(test) + 1] <- paste0(n_years, "-year trend")
    
    # Don't assume the tail is bringing back all years:
    last_n <- dplyr::filter(gwl, year >= latest_measured_year - n_years)

    # Perform the seasonal Kendall Trend Test on the ten-year data set using 
    # the rkt package.  At the recommendation of Bob Hirsch, rkt is used 
    # b/c it can handle data sets that do not have data for all the seasons 
    # in all years, whereas the EnvStats package cannot. 
    TrendInfo_n <- rkt::rkt(last_n$decYear, 
                            last_n$monthlyMean, 
                            block = last_n$month, 
                            rep = "m",
                            correct = TRUE)
    
    medDateN <- median(last_n$decYear, na.rm = TRUE)
    
    medValueN <- median(last_n$monthlyMean, na.rm = TRUE)
    
    tauN <- TrendInfo_n[["tau"]]
    pValueN <- TrendInfo_n[["sl"]]
    
    if(tauN == 0){ 
      trendN <- "None"
      slopeN <- NA
      interceptN <- NA
    } else if(pValueN >= 0.05){ 
      trendN <- "Not significant"
      slopeN <- NA
      interceptN <- NA
    } else { 
      slopeN <- TrendInfo_n[["B"]]
      trendN <- ifelse(slopeN > 0, "Up", "Down")
      interceptN <- medValueN - slopeN * medDateN # Manually 
      # calculating intercept b/c rkt package does not provide the intercept
    }
    tau[length(tau) + 1] <- tauN
    pValue[length(pValue) + 1] <- pValueN
    slope[length(slope) + 1] <- slopeN
    intercept[length(intercept) + 1] <- interceptN
    trend[length(trend) + 1] <- trendN
  } else {
    tau[length(tau) + 1] <- NA
    pValue[length(pValue) + 1] <- NA
    slope[length(slope) + 1] <- NA
    intercept[length(intercept) + 1] <- NA
    trend[length(trend) + 1] <- "Insufficient data"
  } 
  
  if(enough_data_por) {
    
    test[length(test) + 1] <- "Period of record"
    medDatePR <- median(gwl$decYear, na.rm = TRUE)
    medValuePR <- median(gwl$monthlyMean, na.rm = TRUE)
    
    # Perform the seasonal Kendall Trend Test on the period of record data set using 
    # the rkt package.  At the recommendation of Bob Hirsch, rkt is used 
    # b/c it can handle data sets that do not have data for all the seasons 
    # in all years, whereas EnvStats package cannot 
    prTrendInfo <- rkt::rkt(gwl$decYear, 
                            gwl$monthlyMean, 
                            block = gwl$month,
                            rep = "m",
                            correct = TRUE)
    
    tauPR <- prTrendInfo[["tau"]]
    pValuePR <- prTrendInfo[["sl"]]
    sampleSizePR <- nrow(gwl)
    if(tauPR == 0){ 
      trendPR <- "None"
      slopePR <- NA
      interceptPR <- NA
    } else if(pValuePR >= 0.05){ 
      trendPR <- "Not significant"
      slopePR <- NA
      interceptPR <- NA
    } else { 
      slopePR <- prTrendInfo[["B"]]
      trendPR <- ifelse(slopePR > 0, "Up", "Down")
      interceptPR <- medValuePR - slopePR * medDatePR # Manually 
      # calculating intercept b/c rkt doesn't provide intercept
    }
    tau[length(tau) + 1] <- tauPR
    pValue[length(pValue) + 1] <- pValuePR
    slope[length(slope) + 1] <- slopePR
    intercept[length(intercept) + 1] <- interceptPR
    trend[length(trend) + 1] <- trendPR
  } else {

    tau[length(tau) + 1] <- NA
    pValue[length(pValue) + 1] <- NA
    slope[length(slope) + 1] <- NA
    intercept[length(intercept) + 1] <- NA
    trend[length(trend) + 1] <- "Insufficient data"
  }
  
  test_results <- data.frame(test,
                             tau,
                             pValue, 
                             slope, 
                             intercept, 
                             trend,
                             stringsAsFactors = FALSE)

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
#' @param days_required_per_month integer. Number of days required per month. 
#' Default is 14.
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
                         value_col = "X_62610_00001",
                         days_required_per_month = 14){
  
  days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  x$result <- x[[value_col]]
  x$date <- x[[date_col]]
  
  monthly_mean <- x %>% 
    dplyr::mutate(month = as.numeric(format(date, "%m")),
           year = as.numeric(format(date, "%Y"))) %>% 
    dplyr::group_by(year, month) %>% 
    dplyr::summarize(mean_va = mean(result, na.rm = TRUE), # should user get to pick median?
              n_days = dplyr::n()) %>% 
    dplyr::filter(n_days > !!days_required_per_month) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(mid_date = as.Date(paste(year, month, 15, sep = "-")))
    
  return(monthly_mean)
  
}

enough_data <- function(x, 
                        n_years = 5, 
                        pctComplete = 0.5,
                        por = FALSE){

  if(por){
    
    monthlyMeansLast_n <- x
    
  } else {
    monthlyMeansLast_n <- x %>%
      dplyr::filter(year >= n_years)
  }
  
  yearly_count <- diff(range(monthlyMeansLast_n$year))
  
  enoughData_n <- yearly_count >= n_years & 
    nrow(monthlyMeansLast_n) > pctComplete * 120 &
    max(monthlyMeansLast_n$gap, na.rm = TRUE) < 10/3
 
  if(yearly_count < n_years) {
    message("Total data time span is less than ", n_years," years")

  } else {

    if(! nrow(monthlyMeansLast_n) > pctComplete * 120) {
      message("Not enough measurements in each of the the last ", n_years,
              " years to proceed")
    } 
  }
  return(enoughData_n)
}

decimalDate <- function(rawData){
  
  dateTime <- as.POSIXlt(rawData)
  year <- dateTime$year + 1900
  
  startYear <- as.POSIXct(paste0(year,"-01-01 00:00"))
  endYear <- as.POSIXct(paste0(year+1,"-01-01 00:00"))
  
  DecYear <- year + as.numeric(difftime(dateTime, startYear, units = "secs"))/as.numeric(difftime(endYear, startYear, units = "secs"))
  return(DecYear)
}