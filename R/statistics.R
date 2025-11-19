#' Trend Test
#' 
#' Test for period of record and user-specified ranges. The default
#' trends are calculated for 10 year and the full period of record.
#' 
#' For data that is at least on a daily interval, the \link[rkt]{rkt} function
#' is used. For periodic data, the \link[EnvStats]{kendallTrendTest} is used.
#' 
#' @inheritParams monthly_frequency_table
#' @param days_required_per_month integer. Number of days required per month
#' to include in the trend test. Default is 14. 
#' @param n_years integer. This is the number of years to calculate the trend on.
#' Default is 10. This can be a vector of years.
#' @param pctComplete number percentage complete. This is a fraction that represents
#' the amount of data that must be included overall in order to calculate a trend.
#' The default is 0.5, which means if gaps in the data span more than 50% of the
#' total record, a trend will not be calculated.
#' @param POR_trend a logical indicating whether to include a trend test
#' for the full period of record. Default is \code{TRUE}.
#' @importFrom stats as.formula
#' @return a data frame of test results from 5 and 20 year Kendall Seasonal Trend test
#' 
#' @export
#' 
#' @examples 
#' 
#' site <- "USGS-263819081585801"
#' p_code_dv <- "62610"
#' statCd <- "00001"
#' 
#' # Using package example data:
#' # gwl_data <- dataRetrieval::read_waterdata_field_measurements(monitoring_location_id = site, 
#' #                                  skipGeometry = TRUE)
#' gwl_data <- L2701_example_data$Discrete
#'                         
#' # gw_level_dv <- dataRetrieval::read_waterdata_daily(monitoring_location_id = site,
#' #                                                    parameter_code = p_code_dv,
#' #                                                    statistic_id = statCd,
#' #                                                    skipGeometry = TRUE)
#' #
#'                                                     
#' gw_level_dv <- L2701_example_data$Daily
#' 
#' trend_test(gw_level_dv,
#'            gwl_data,
#'            parameter_cd = "62610")
#'            
#' trend_test(gw_level_dv,
#'            gwl_data,
#'            POR_trend = FALSE,
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
#'            n_years = c(5, 10, 20))
#'
#' # Only periodic data:
#' trend_test(NULL,
#'            gwl_data,
#'            parameter_cd = "62610")
#'            
trend_test <- function(gw_level_dv, 
                       gwl_data,
                       n_years = 10,
                       parameter_cd = NA,
                       date_col = c("time", "time"),
                       value_col = c("value", "value"),
                       approved_col = c("approval_status", "approval_status"),
                       pctComplete = 0.5,
                       days_required_per_month = 14,
                       POR_trend = TRUE) {

  data_list <- set_up_data(gw_level_dv = gw_level_dv, 
                           gwl_data = gwl_data, 
                           parameter_cd = parameter_cd,
                           date_col = date_col,
                           value_col = value_col, 
                           approved_col = approved_col)
  
  gw_level_dv <- data_list$gw_level_dv
  gwl_data <- data_list$gwl_data

  only_periodic <- nrow(gw_level_dv) == 0
  
  gwl <- dplyr::bind_rows(gw_level_dv,
                          gwl_data) |>
    dplyr::mutate(year = as.numeric(format(Date, "%Y")),
                  month = as.numeric(format(Date, "%m")),
                  doy = as.numeric(format(Date, "%j"))) |>
    dplyr::group_by(year, month) |>
    dplyr::summarize(monthlyMean = mean(Value, na.rm = TRUE),
                     ndays = length(doy)) |> 
    dplyr::ungroup() |>
    dplyr::mutate(midMonth = as.Date(sprintf("%s-%s-15", year, month)),
                  decYear = decimalDate(midMonth),
                  gap = decYear - dplyr::lag(decYear) >= 1000)  #this gets rid of nonsense first value

  latest_measured_year <- max(gwl$year, na.rm = TRUE)

  enough_data_por <- enough_data(gwl,
                                 only_periodic,
                                 n_years = 11,
                                 pctComplete = pctComplete,
                                 por = TRUE) # 50% of monthly data

  test <- vector()
  tau <- vector()
  pValue <- vector()
  slope <- vector()
  intercept <- vector()
  trend <- vector()
  
  for(year in n_years){
    
    enough_data_n <- enough_data(gwl,
                                 only_periodic,
                                 pctComplete = pctComplete,
                                 n_years = year) 
  
    if(enough_data_n) {
      test[length(test) + 1] <- paste0(year, "-year trend")
      
      # Consider making this a vector of years
      
      # Don't assume the tail is bringing back all years:
      last_n <- dplyr::filter(gwl,
                              year >= latest_measured_year - !!year)
  
      if(only_periodic){
        # Perform the Kendall Trend Test on the ten-year data set using 
        # the envstats package. Not doing a seasonal test on the periodic data
        # b/c the data tend to be collected at the same time of year.
        TrendInfo_n <- EnvStats::kendallTrendTest(monthlyMean ~ decYear,
                                             data = last_n)
        
        # Pull the results from the Kendall Trend test that are needed 
        # for the plots and tables
        tauN <- TrendInfo_n[["estimate"]][["tau"]]
        pValueN <- TrendInfo_n[["p.value"]]
        
        if(tauN == 0){ 
          trendN <- "None"
          slopeN <- NA
          interceptN <- NA
        } else if(pValueN >= 0.05){ 
          trendN <- "Not significant"
          slopeN <- NA
          interceptN <- NA
        } else {
          slopeN <- TrendInfo_n[["estimate"]][["slope"]]
          trendN <- ifelse(slopeN > 0, "Up", "Down")
          interceptN <- TrendInfo_n[["estimate"]][["intercept"]]
        } 
        
      } else {
        
        last_n <- last_n |> 
          dplyr::filter(ndays > !!days_required_per_month)
        
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
      }
      
      tau[length(tau) + 1] <- tauN
      pValue[length(pValue) + 1] <- pValueN
      slope[length(slope) + 1] <- slopeN
      intercept[length(intercept) + 1] <- interceptN
      trend[length(trend) + 1] <- trendN
    } else {
      test[length(test) + 1] <- paste0(year, "-year trend")
      tau[length(tau) + 1] <- NA
      pValue[length(pValue) + 1] <- NA
      slope[length(slope) + 1] <- NA
      intercept[length(intercept) + 1] <- NA
      trend[length(trend) + 1] <- "Insufficient data"
    } 
  }
  
  if(enough_data_por & POR_trend) {
    test[length(test) + 1] <- "Period of record"
    
    if(only_periodic){
      # Perform the Kendall Trend Test on the ten-year data set using 
      # the envstats package. Not doing a seasonal test on the periodic data
      # b/c the data tend to be collected at the same time of year.
      TrendInfo_n <- EnvStats::kendallTrendTest(monthlyMean ~ decYear,
                                                data = gwl)
      
      # Pull the results from the Kendall Trend test that are needed 
      # for the plots and tables
      tauPR <- TrendInfo_n[["estimate"]][["tau"]]
      pValuePR <- TrendInfo_n[["p.value"]]
      
      if(tauPR == 0){ 
        trendPR <- "None"
        slopePR <- NA
        interceptPR <- NA
      } else if(pValuePR >= 0.05){ 
        trendPR <- "Not significant"
        slopePR <- NA
        interceptPR <- NA
      } else {
        slopePR <- TrendInfo_n[["estimate"]][["slope"]]
        trendPR <- ifelse(slopePR > 0, "Up", "Down")
        interceptPR <- TrendInfo_n[["estimate"]][["intercept"]]
      } 
      
    } else {
    
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
    }
    tau[length(tau) + 1] <- tauPR
    pValue[length(pValue) + 1] <- pValuePR
    slope[length(slope) + 1] <- slopePR
    intercept[length(intercept) + 1] <- interceptPR
    trend[length(trend) + 1] <- trendPR
  } else if (POR_trend) {
    test[length(test) + 1] <- "Period of record"
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
#' @export
#' @examples 
#' 
#' # site <- "USGS-263819081585801"
#' p_code_dv <- "62610"
#' # statCd <- "00001"
#' # gw_level_dv <- dataRetrieval::read_waterdata_daily(monitoring_location_id = site,
#' #                                                    parameter_code = p_code_dv,
#' #                                                    statistic_id = statCd,
#' #                                                    skipGeometry = TRUE)
#' gw_level_dv <- L2701_example_data$Daily
#' 
#' site_statistics <- monthly_frequency_table(gw_level_dv, 
#'                                            NULL,
#'                                            parameter_cd = p_code_dv)
#' 
#' gw_monthly <- monthly_mean(gw_level_dv)
#' 
monthly_mean <- function(x,
                         date_col = "time",
                         value_col = "value"){
  
  days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  x$result <- x[[value_col]]
  x$date <- x[[date_col]]
  
  monthly_mean <- x |> 
    dplyr::mutate(month = as.numeric(format(date, "%m")),
           year = as.numeric(format(date, "%Y"))) |> 
    dplyr::group_by(year, month) |> 
    dplyr::summarize(mean_va = mean(result, na.rm = TRUE), # should user get to pick median?
              n_days = dplyr::n()) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(mid_date = as.Date(paste(year, month, 15, sep = "-")))
    
  return(monthly_mean)
  
}

enough_data <- function(x, 
                        periodic,
                        n_years = 5, 
                        pctComplete = 0.5,
                        por = FALSE){

  if(por){
    monthlyMeansLast_n <- x
  } else {
    monthlyMeansLast_n <- x |>
      dplyr::filter(year >= n_years)
  }
  
  yearly_count <- diff(range(monthlyMeansLast_n$year))

  if(periodic){
    #Check if data are adequate for a ten-year trend analysis.  
    # A) Does the dataset go back at least n years, B) are there at least n
    # data points available over the last n years, and C) are there any data 
    # gaps that are longer than 1/3 of the trend period?
    enoughData_n <- yearly_count >= n_years & 
      nrow(monthlyMeansLast_n) >= n_years & 
      max(monthlyMeansLast_n$gap, na.rm = TRUE) < n_years/3
  } else {
    #Check if data are adequate for a n-year trend analysis.  
    # A) Does the dataset go back at least n years, B) are there at least 10
    # data points available over the last n years, and C) are there any data 
    # gaps that are longer than 1/3 of the trend period?  
    enoughData_n <- yearly_count >= n_years & 
      nrow(monthlyMeansLast_n) > pctComplete * 12 * yearly_count &
      max(monthlyMeansLast_n$gap, na.rm = TRUE) < yearly_count/3
    
  }
  
  if(por & yearly_count < 12){
    message("Period of record is less than 12 years, trend not calculated.")
    enoughData_n <- FALSE
  }
  
  if(yearly_count < n_years) {
    message("Total data time span is less than ", n_years," years")

  } else {
    if(! nrow(monthlyMeansLast_n) > pctComplete * 12 * n_years) {
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
  
  DecYear <- year + as.numeric(difftime(dateTime, startYear, units = "secs")) / 
    as.numeric(difftime(endYear, startYear, units = "secs"))
  return(DecYear)
}