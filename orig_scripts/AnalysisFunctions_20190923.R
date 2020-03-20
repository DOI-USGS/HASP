
# ____________________________________________________________________________________________________
# ****************************************************************************************************
# ****************              FUNCTION - markerLookup                    ***************************
# ****************                                                         ***************************
# ****************************************************************************************************
#
markerLookup <- function(inputTrend){
  markerTable <- read.csv(paste(MainDir,'InputFiles/GenericMarkerLookupTable.csv', sep = ""), 
                          stringsAsFactors = FALSE)
  markerLookupSubset <- markerTable %>% 
    filter(Trend == inputTrend)
  markerDescription <- markerLookupSubset$MarkerDescription
  return(markerDescription)
}

# END FUNCTION - markerLookup

# ____________________________________________________________________________________________________
# ****************************************************************************************************
# ****************                  FUNCTION getDVData                     ***************************
# ****************                                                         ***************************
# ****************         Table of DV values modified for analysis        ***************************
# ****************************************************************************************************
#
  # WHAT IS NEEDED BY THIS FUNCTION?
  # current_site, pCode, statCd
  #
  # WHAT IS OUTPUT BY THIS FUNCTION? 
  # A table of daily values for the current_site, parameter code, and statCd, that has been rearrange for analysis

getDVData <- function(current_site, pCode, statCd){
  
  
  gwlevel <- readNWISdv(siteNumber = current_site, parameterCd = pCode, statCd = statCd)
  
  # If gwlevel is empty skip all the following steps and indicate its empty
  if (nrow(gwlevel) > 0){
    
    # rename columns
    # THIS SECTION COULD BE PROBLEMATIC FOR SW SITES THERE IS NOT ONLY ONE COLUMN OF VALUES
    gwlevel <- renameNWISColumns(gwlevel)
    
    
    
    # Create columns in gwlevel with month, day of year, year, week of year, the abbreviated month
    
    gwlevel$month <- as.POSIXlt(gwlevel$Date)$mon + 1
    gwlevel$doy <- as.POSIXlt(gwlevel$Date)$yday + 1
    gwlevel$year <- as.POSIXlt(gwlevel$Date)$year + 1900
    gwlevel$NewDate <- as.Date(gwlevel$Date, format="%d-%m-%Y")
    #gwlevel$woy <- as.numeric( format(gwlevel$NewDate+0, "%U"))
    gwlevel$woy <-as.numeric(strftime(gwlevel$NewDate,format="%W")) 
    gwlevel$woy[gwlevel$woy==0]<-1
    gwlevel$woy[gwlevel$woy==53]<-52
    gwlevel$AbbMonth <- month.abb[gwlevel$month]
    
    # Convert Abbreviated months to USGS abbreviated months
    gwlevel$AbbMonth[gwlevel$AbbMonth=="Jan"]<-"Jan."
    gwlevel$AbbMonth[gwlevel$AbbMonth=="Feb"]<-"Feb."
    gwlevel$AbbMonth[gwlevel$AbbMonth=="Mar"]<-"Mar."
    gwlevel$AbbMonth[gwlevel$AbbMonth=="Apr"]<-"Apr."
    gwlevel$AbbMonth[gwlevel$AbbMonth=="May"]<-"May"
    gwlevel$AbbMonth[gwlevel$AbbMonth=="Jun"]<-"June"
    gwlevel$AbbMonth[gwlevel$AbbMonth=="Jul"]<-"July"
    gwlevel$AbbMonth[gwlevel$AbbMonth=="Aug"]<-"Aug."
    gwlevel$AbbMonth[gwlevel$AbbMonth=="Sep"]<-"Sept."
    gwlevel$AbbMonth[gwlevel$AbbMonth=="Oct"]<-"Oct."
    gwlevel$AbbMonth[gwlevel$AbbMonth=="Nov"]<-"Nov."
    gwlevel$AbbMonth[gwlevel$AbbMonth=="Dec"]<-"Dec."
  }
  
  return(gwlevel)
}
  
# ____________________________________________________________________________________________________
# ****************************************************************************************************
# ****************                  FUNCTION checkDVData                   ***************************
# ****************                                                         ***************************
# ****************         Table of DV values modified for analysis        ***************************
# ****************************************************************************************************
  # WHAT IS NEEDED BY THIS FUNCTION?
  # gwlevel
  #
  # WHAT IS OUTPUT BY THIS FUNCTION? 
  # A dataframe that includes variables describing if there are any data available, as well if there are any data for the current year or the previous, year. 
  # If there are data available for either this year or last year the script quantifies how much, which is expressed as a percentage of complete record, given 
  # the current date. So for example if has only been 10 days in the current water year, and there are 10 values the record would be 100% for the current year.
  # The script also outputs a number of dates which can be used elsewhere in the program.


checkDVData <- function(gwlevel){
  
  
  
  #*************************************************************************************
  #*********   CHECK TO MAKE SURE THERE IS SUFFICIENT DATA FOR THE SCRIPT   ************
  #*************************************************************************************
  
  # Determine the date of the last reading, the value of last reading, and how old it is.
  
  if (nrow(gwlevel) > 0){
    IsAnyDataAvailable <-"YES"
    # SET UP THE TESTING VARIABLES
    
    # Find out the date and value of the last reading
    dateLastReading<- as.POSIXlt(max(gwlevel$Date, na.rm = TRUE))
    woyLastReading<-as.numeric( strftime(as.POSIXlt(dateLastReading),format="%W")) # Week of year for the last value
    doyLastReading<-as.numeric( as.POSIXlt(dateLastReading)$yday + 1) # Day of the year of the last value
    LastReading<-gwlevel%>% 
      filter(Date==dateLastReading)
    
    # Determine how old the last reading is
    currentnessOfData<-as.numeric(as.Date(today())-as.Date(dateLastReading))
    # Determine the value of the last reading
    valueLastReading <- round(LastReading$value, digits = 2)
    
    # determine the current date, day, and week
    Cdate<-Sys.Date()
    currentDay<-as.numeric( format(Cdate, "%d"))
    currentWeek <-as.numeric( strftime(as.POSIXlt(Cdate),format="%W"))
    currentYear <-as.numeric( strftime(as.POSIXlt(Cdate),format="%Y"))
    currentMonth <-as.numeric(strftime(as.POSIXlt(Cdate),format="%m"))
    if (currentMonth>= 9){ 
      startCurrentWY<-as.Date(paste0(as.character(currentYear-1),"-10-01"))
      startPreviousWY<-as.Date(paste0(as.character(currentYear-2),"-10-01"))
    }else{
      startCurrentWY<-as.Date(paste0(as.character(currentYear-0),"-10-01"))
      startPreviousWY<-as.Date(paste0(as.character(currentYear-1),"-10-01"))
    }
    lastDate<-gwlevel$Date[length(gwlevel$site_no)]
    
    # TEST TO SEE IF THERE IS ANY DATA FOR THE CURRENT YEAR OR LAST YEAR
    
    if(IsAnyDataAvailable == "YES"){
      # Evaluate data for the current year to make sure that its not all NA
      # If there is data determine how much. Set a flag to indicate if its all NA or not
      LastYearValues<- gwlevel %>% 
        filter(Date >=startCurrentWY)
      LastYearValues <- within(LastYearValues, testNa <- (!is.na(LastYearValues$value)))
      LastYearValuesNA <-LastYearValues %>% filter(testNa == TRUE)
      daysCurYr2Date<-as.numeric(Cdate - startCurrentWY)
      daysComputedCurYr<-as.numeric(length(LastYearValuesNA$value))
      prctRecordCuryr<-daysComputedCurYr/daysCurYr2Date*100
      if(length(LastYearValuesNA$value)>0){IsThereDataCurrYr<-"YES"}else{IsThereDataCurrYr<-"NO"}
    }
    
    if(IsAnyDataAvailable == "YES"){
      # Evaluate data for the previous year to make sure that its not all NA
      # If there is data determine how much. Set a flag to indicate if its all NA or not
      PreviousYearValues<- gwlevel %>% 
        filter(Date >=startPreviousWY)%>% 
        filter(Date < startCurrentWY)
      PreviousYearValues <- within(PreviousYearValues, testNa <- (!is.na(PreviousYearValues$value)))
      PreviousYearValuesNA <-PreviousYearValues %>% filter(testNa == TRUE)
      daysComputedPrevYr<-as.numeric(length(PreviousYearValuesNA$value))
      prctRecordPrevyr<-daysComputedPrevYr/365.25*100
      if(length(PreviousYearValuesNA$value)>0){IsThereDataPrevYr<-"YES"}else{IsThereDataPrevYr<-"NO"}
      
    }
  }else{
    IsAnyDataAvailable <-"NO"
    dateLastReading<-NA 
    currentYear<- NA 
    currentMonth<-NA 
    currentDay<-NA 
    currentWeek<-NA  
    currentnessOfData<-NA
    startCurrentWY<-NA 
    valueLastReading<-NA
    IsThereDataCurrYr<-NA
    prctRecordCuryr<-NA
    IsThereDataPrevYr<-NA
    prctRecordPrevyr<-NA
    Cdate<-NA
    doyLastReading<-NA
    woyLastReading<-NA
  }
    
    # Create a dataframe that has the output from this testing script
    
  checkResults<-data.frame(dateLastReading, currentYear, currentMonth, currentDay, currentWeek,  currentnessOfData, startCurrentWY, valueLastReading, IsAnyDataAvailable, IsThereDataCurrYr, prctRecordCuryr, IsThereDataPrevYr, prctRecordPrevyr, Cdate, doyLastReading, woyLastReading)
  
  return(checkResults)
  
}
  

# ____________________________________________________________________________________________________
# ****************************************************************************************************
# ****************            FUNCTION  computeDates                       ***************************
# ****************                                                         ***************************
# ****************         Computes dates needed for plots                 ***************************
# ****************************************************************************************************
  #
  # WHAT IS NEEDED BY THIS FUNCTION?
  # checkResults$Cdate, checkResults$currentMonth
  #
  # WHAT IS OUTPUT BY THIS FUNCTION? 
  # startDate, startDate2yr, EndDate, startWeek, WoyLastReading,doyLastReading, DateLastMonth, DateLastWeek

  computeDates <- function(checkResults){
  
  # _____________________________________________________________________________________
  #**************************************************************************************
  #***********         Determine start and end Dates for Plots        *******************
  #**************************************************************************************
  
  # What input variables are needed
  # 
  #
  # What variables are created startDate, startDate2yr, EndDate, startWeek, WoyLastReading,doyLastReading, DateLastMonth, DateLastWeek  
  
  if(checkResults$currentMonth <12){startMonth<- as.POSIXlt(checkResults$Cdate)$mon + 2} else {startMonth<- as.POSIXlt(checkResults$Cdate)$mon -10}
  if(checkResults$currentMonth <12){startYear<-as.POSIXlt(checkResults$Cdate)$year + 1900-1} else {startYear<-as.POSIXlt(checkResults$Cdate)$year + 1900}
  startDay<-1
  startDate<-as.Date(paste0(startYear,"-",startMonth,"-", startDay))
  startDate2yr<-as.Date(paste0(startYear-1,"-",startMonth,"-", startDay))
  EndDate<-as.Date(paste0(startYear+1,"-",startMonth,"-", startDay))
  startWeek<-strftime(startDate, format="%W")
  startWeek<-as.numeric(startWeek)
  # 
  
  # Determine the dates of last weeks reading and last months reading
  WoyLastReading <- checkResults$woyLastReading   
  doyLastReading <- checkResults$doyLastReading   
  # Determine the date of last months reading
  
  d <- as.Date(checkResults$dateLastReading)
  if(month(d)>1){month(d) <- month(d) - 1}else{
    month(d) <-12
    year(d)<-year(d)-1
  }
  DateLastMonth <-d
  
  # Posixlt dates are in seconds so I have to subtract 86,400 seconds to subtract a day, and 604,800 seconds to subtract a week
  
  DateLastWeek <- as.POSIXlt(checkResults$dateLastReading) - 604800
  
  # End of computing dates
  computedDates<-data.frame(startDate, startDate2yr, EndDate, startWeek, WoyLastReading,doyLastReading, DateLastMonth, DateLastWeek, startMonth)
  #End of function
  return(computedDates)
}
  

  ###############################################################################
  #                                                                             #
  ###################     FUNCTION COMPUTESKTT     ##############################
  #                                                                             #
  ###############################################################################
  
  
  computeSKTT<-function(gwlevel, trendPeriods){
    
    # This fuction takes the daily value data proceeed by preceeding functions
    # and computes the Season Kedall Trend Test for one or more trend periods
    #
    # WHAT IS NEEDED BY THIS FUNCTION?
    # gwlevel, trendPeriods
    #
    # WHAT IS OUTPUT BY THIS FUNCTION? 
    # A data frame with the trend results for the 
    
    
    # END SET UP
    
    
    ###############################################################################
    #                                                                             #
    ###################  Seasonal Kendall Trend Test ##############################
    #                                                                             #
    ###############################################################################
    
    #
    # WHAT IS NEEDED BY THIS FUNCTION?
    # gwlevel, trendPeriod
    #
    # WHAT IS OUTPUT BY THIS FUNCTION? 
    # A dataframe with the trend test results
    
    # THIS CODE BLOCK IS READY TO TURN INTO A FUNTION IT WILL REPLACE THE NEED
    # FOR A SEPARATE FIVE YEAR AND TWENTY YEAR TREND ANALYSIS CODE BLOCK
    # AND NOW THE TREND PERIODS CAN BE SPECIFIED TO PERIOD DESIRED.
    # 
    # THE KENDAL TREND TEST CODE HOWEVER HAD TO BE PULLED OUT AND NEEDS TO BE  
    # MADE INTO A SEPERATE FUNCTION
    
    
    # Compute monthy means 
    
    # Set up a couple variables
    trendPeriods<-c(5,20)
    
    # Remove any former versions of TrendTest Results
    if(exists("TrendTestResults")){rm(TrendTestResults)}
    
    # Compute the monthly means and censor out any months with less than 14 days
    MonthlyMeans<-gwlevel %>%
      group_by(year,month)%>%
      summarize(
        mean(value, na.rm = TRUE),
        nDays = length(doy)
      )%>%
      rename("meanWL" = "mean(value, na.rm = TRUE)")%>%
      rename("ndays" = "nDays")
    # censor the means of any months that are based on less than 15 days of data
    MonthlyMeansCensored<-MonthlyMeans %>%
      filter(ndays>14)
    # determine how many years of data are available for analysis
    YearsOfRecord<-length(MonthlyMeansCensored$month)/12
    
    # Set up an empty table for results
    counter <- 0
    for (yrs in trendPeriods){
      
      counter <- counter+1
      Period<-numbers2words(yrs)
      
      
      # Build a table that will house the trend test results
      
      if(counter == 1){
        Results<-c(rep(NA, 5))
        TrendTestResults<-Results
        names(TrendTestResults)<-c("tau", "pValue", "slope", "intercept", "trend")
        colText<-paste0(Period,"YearResults")
      }else{
        TrendTestResults<-rbind(TrendTestResults, Results)
        colTextNew<-paste0(Period,"YearResults")
        colText<-c(colText,colTextNew)
      }
    }
    row.names(TrendTestResults)<-colText
    TrendTestResults<-data.frame(TrendTestResults)
    
    # Compute trend test and determine statistics for each trend
    
    counter <- 0
    for (yrs in trendPeriods){
      counter <- counter+1
      
      # Code was originally designed to test for 5 year trends
      # By converting it into a function it can now be used to compute
      # Trend for any period
      # is there a trend? 
      # is it statistically significant? 
      # is it positive or negative?
      
      if(YearsOfRecord > yrs){
        MonthlyMeansLast5Yrs<-MonthlyMeansCensored %>%
          filter(year>=(as.numeric(checkResults$currentYear)-yrs))
        FiveYearTrendInfo <- kendallSeasonalTrendTest(meanWL ~ month + year, data = MonthlyMeansLast5Yrs)
        TrendTestResults$tau[counter] = FiveYearTrendInfo$estimate['tau']
        TrendTestResults$pValue[counter] = FiveYearTrendInfo$p.value[2]
        TrendTestResults$slope[counter] = FiveYearTrendInfo$estimate['slope']
        TrendTestResults$intercept[counter] = FiveYearTrendInfo$estimate['intercept']
        if(TrendTestResults$tau[counter] == 0){ 
          TrendTestResults$trend[counter] <- "None"
        } else if(TrendTestResults$pValue[counter] >= 0.05){ 
          TrendTestResults$trend[counter] <- "Not significant"
        } else { 
          TrendTestResults$trend[counter] <- ifelse(TrendTestResults$slope[counter] > 0, "Up", "Down")
        }
      }else{TrendTestResults$trend[counter]<-"Insufficient data"}
      
    }
    
    # End ComputeSKTT
    #**********************************************************************************
    #**********************************************************************************
    #***********************************************************************************
    
    return(TrendTestResults)
  }
  
   
  
  
  
  
    
# ____________________________________________________________________________________________________
# ****************************************************************************************************
# ****************                FUNCTION   ASSIGNTLSMBL                  ***************************
# ****************                                                         ***************************
# ****************         Assign a symbol for the long term trend         ***************************
# ****************************************************************************************************
  #
  # WHAT IS NEEDED BY THIS FUNCTION?
  # The data.frame TrendTestResults
  #
  # WHAT IS OUTPUT BY THIS FUNCTION? 
  # Text describing the long term trend symbol
  # 
  assignLTSmbl<-function(TrendTestResults){
  
  #*********************************************************************************************
  #******************* Based on Results of Trend Test Assign LT Trend Symbol *******************
  #*********************************************************************************************
  
  
  finalLTSymbol<-"Error symbol not assigned properly"
  
  if(TrendTestResults$trend[1]==TrendTestResults$trend[2]){
    FinalLTTrend<-TrendTestResults$trend[1]
    if(FinalLTTrend=="Up"){finalLTSymbol<-"FatUpArrow"}
    if(FinalLTTrend=="Down"){finalLTSymbol<-"FatDownArrow"}
    if(FinalLTTrend=="Not significant"){finalLTSymbol<-"Circle"}
    if(FinalLTTrend=="None"){finalLTSymbol<-"Circle"}
    if(FinalLTTrend=="Insufficient data"){finalLTSymbol<-"Square"}
  }else{
    if(TrendTestResults$trend[1]=="Up" && TrendTestResults$trend[2]=="Down"){finalLTSymbol<-"DoubleHeadedArrow"}
    if(TrendTestResults$trend[1]=="Down" && TrendTestResults$trend[2]=="Up"){finalLTSymbol<-"DoubleHeadedArrow"}
    
    if(TrendTestResults$trend[1]=="Up" && TrendTestResults$trend[2]=="Not significant"){finalLTSymbol<-"ThinUpArrow"}
    if(TrendTestResults$trend[1]=="Down" && TrendTestResults$trend[2]=="Not significant"){finalLTSymbol<-"ThinDownArrow"}
    
    if(TrendTestResults$trend[1]=="Up" && TrendTestResults$trend[2]=="None"){finalLTSymbol<-"ThinUpArrow"}
    if(TrendTestResults$trend[1]=="Down" && TrendTestResults$trend[2]=="None"){finalLTSymbol<-"ThinDownArrow"}
    
    if(TrendTestResults$trend[1]=="Up" && TrendTestResults$trend[2]=="Insufficient data"){finalLTSymbol<-"ThinUpArrow"}
    if(TrendTestResults$trend[1]=="Down" && TrendTestResults$trend[2]=="Insufficient data"){finalLTSymbol<-"ThinDownArrow"}
    
    if(TrendTestResults$trend[2]=="Up" && TrendTestResults$trend[1]=="Not significant"){finalLTSymbol<-"FatUpArrow"}
    if(TrendTestResults$trend[2]=="Down" && TrendTestResults$trend[1]=="Not significant"){finalLTSymbol<-"FatDownArrow"}
    
    if(TrendTestResults$trend[2]=="Up" && TrendTestResults$trend[1]=="None"){finalLTSymbol<-"FatUpArrow"}
    if(TrendTestResults$trend[2]=="Down" && TrendTestResults$trend[1]=="None"){finalLTSymbol<-"FatDownArrow"}
    
    if(TrendTestResults$trend[2]=="Up" && TrendTestResults$trend[1]=="Insufficient data"){finalLTSymbol<-"FatUpArrow"}
    if(TrendTestResults$trend[2]=="Down" && TrendTestResults$trend[1]=="Insufficient data"){finalLTSymbol<-"FatDownArrow"}
    
    if(TrendTestResults$trend[1]=="Not significant" && TrendTestResults$trend[2]=="Insufficient data"){finalLTSymbol<-"Circle"}
    if(TrendTestResults$trend[1]=="None" && TrendTestResults$trend[2]=="Insufficient data"){finalLTSymbol<-"Circle"}
    if(TrendTestResults$trend[1]=="None" && TrendTestResults$trend[2]=="Not significant"){finalLTSymbol<-"Circle"}
  }
  
  
  #*********************************************************************************************
  #************************* End of Assigning Symbol for LT trend  *****************************
  #*********************************************************************************************    
  
  return(finalLTSymbol)
  
  #End of function
} 
  
  ###############################################################################
  #                                                                             #
  ###################   FUNCTION TRENDSANDCHANGES        ########################
  #                                                                             #
  ###############################################################################
  #******************************************************************************
  #*********    Based on Results of trend tests and comparisons      ************
  #*********      assign trend symbols and compute changes           ************     
  #******************************************************************************
  
  trendsAndChanges<-function(checkResults, LastWeeksRecord, LastMonthsRecord){      
    #
    # WHAT IS NEEDED BY THIS FUNCTION?
    # checkResults, LastWeeksRecord, LastMonthsRecord
    #
    # WHAT IS OUTPUT BY THIS FUNCTION? 
    # a data.frame with information describing trend symbols and changes 
    # 
    # -------------------------------------------------------------------------------------------------
    #__________________________________________________________________________________________________
    # DETERMINE CHANGE IN LAST WEEK and SET UP ARROWS FOR MAP
    #
    
    if(length(LastWeeksRecord$value)==0){
      
      WLsymbol <- "Square"
      WLChangeDirWeek <- "Insufficient data"
      WLChangeLastWeek <- NA
      
    }else{
      if(is.na(checkResults$valueLastReading) || is.na(LastWeeksRecord$value)){
        WLsymbol <- "Square"
        WLChangeDirWeek <- "Insufficient data"
        WLChangeLastWeek <- NA} else{
          
          if(checkResults$currentnessOfData>2){
            WLsymbol <- "Square"
            WLChangeDirWeek <- "Insufficient data"
            WLChangeLastWeek <- NA
          } else{
            WLChangeLastWeek <- round(checkResults$valueLastReading-LastWeeksRecord$value, digits = 2)
            
            
            if(WLChangeLastWeek < 0 ){
              WLChangeDirWeek <- "down"
              WLsymbol <- "ThinDownArrow"
            }
            if(WLChangeLastWeek == 0 ){
              WLChangeDirWeek <- "no change"
              WLsymbol <- "Circle"
            }
            if(WLChangeLastWeek > 0 ){
              WLChangeDirWeek <- "up"
              WLsymbol <- "ThinUpArrow"
            }
            
          }
          
          
          
        } 
      
    }
    
    # END SETTING UP ARROWS
    
    
    # DETERMINE CHANGE IN the LAST Month and SET UP ARROWS FOR MAP
    #
    if(length(LastMonthsRecord$value)==0){
      
      WLsymbolM <- "Square"
      WLChangeDurMonth <- "Insufficient data"
      WLChangeLastMonth <- NA
    }else{
      
      if(is.na(checkResults$valueLastReading) || is.na(LastMonthsRecord$value)){
        WLsymbolM <- "Square"
        WLChangeDurMonth <- "Insufficient data"
        WLChangeLastMonth <- NA} else{
          if(checkResults$currentnessOfData>6){
            WLsymbolM <- "Square"
            WLChangeDurMonth<- "Insufficient data"
            WLChangeLastMonth<- NA
          } else{
            WLChangeLastMonth <- round(checkResults$valueLastReading-LastMonthsRecord$value, digits = 2)
            if(WLChangeLastMonth < 0 ){
              WLChangeDurMonth <- "down"
              WLsymbolM <- "ThinDownArrow"
            }
            if(WLChangeLastMonth == 0 ){
              WLChangeDurMonth <- "no change"
              WLsymbolM <- "Circle"
            }
            if(WLChangeLastMonth > 0 ){
              WLChangeDurMonth <- "up"
              WLsymbolM <- "ThinUpArrow"
            }
          }
          
        }
      
      
      
    }
    
    
    changesAndSmybls<-data.frame(cbind(WLsymbol,WLChangeDirWeek,WLChangeLastWeek, WLsymbolM, WLChangeDurMonth,WLChangeLastMonth))
    
    # END SETTING UP ARROWS
    
    return(changesAndSmybls)
  }  
  
# ____________________________________________________________________________________________________
# ****************************************************************************************************
# ****************                FUNCTION   COMPUTESTATS                  ***************************
# ****************                                                         ***************************
# ****************         A dataframe with the computed stats             ***************************
# ****************************************************************************************************

computeStats <- function(gwlevel, MainDir, FullSiteName){
  
  #
  # WHAT IS NEEDED BY THIS FUNCTION?
  # gwlevel, MainDir, FullSiteName
  #
  # WHAT IS OUTPUT BY THIS FUNCTION? 
  # A dataframe with the results of the frequency analysis and other statistics
  # A table printed out to the Table directory named using FullSiteName  
  # FREQUENCY ANALYSIS - Compute the min and max median
  
  annualStats <- gwlevel %>%
    group_by(year,woy) %>%
    summarize(median = median(value)) %>%
    group_by(woy) %>%
    summarise(minMed = min(median, na.rm=TRUE),
              maxMed = max(median, na.rm=TRUE))
  # Its possible that stats may not be computed for every week of the year if there is not enough data. 
  # Need to join annual states with a column that lists every week of year to see what is missing
  
  weeksInYear<-data.frame(c(1:52))
  names(weeksInYear)<-"woy"
  annualStats <- merge(annualStats, weeksInYear, 
                     by="woy", all = TRUE)
  # FREQUENCY ANALSIS - Compute the quantiles
  gwlevelStats <- gwlevel %>%
    group_by(woy) %>%
    summarise(p10 = quantile(value, probs=0.1, na.rm=TRUE),
              p25 = quantile(value, probs=0.25, na.rm=TRUE),
              p50 = quantile(value, probs=0.5, na.rm=TRUE),
              p75 = quantile(value, probs=0.75, na.rm=TRUE),
              p90 = quantile(value, probs=0.9, na.rm=TRUE),
              nYears = length(unique(year)),
              p5 = quantile(value, probs=0.05, na.rm=TRUE),
              p95 = quantile(value, probs=0.95,na.rm=TRUE ))
  
  MainStats <- merge(annualStats, gwlevelStats, 
                     by="woy", all = TRUE) 
  gwlevelStats <- merge(annualStats, gwlevelStats, 
                        by="woy", all = TRUE)
  
  MainStats2<-merge(lastYearofData, MainStats, by="woy", all = TRUE)
  MainStats2<-MainStats2[order(MainStats2$OrgOrder),]
  
  
  finalStats <- gwlevelStats%>%
    select(woy, minMed, p10, p25, p50, p75, p90,  maxMed, nYears)
  
  # Create output table with percentiles
  finalstatsOut<-round(finalStats, digits = 2)
  names(finalstatsOut)<-c("Week of the year", "Lowest median", "10th percentile", "25th percentile", "50th percentile", "75 percentile", "90th percentile", "Highest median", "Number of years")
  PercentileTabPath <- paste0(MainDir,"tables/",FullSiteName,"percentiles",".html")
  print(xtable(finalstatsOut), type="html", file=PercentileTabPath, include.rownames=FALSE)
  # End of creating output table with percentiles
  
  #End of function
  return(finalstatsOut)
} 


# ____________________________________________________________________________________________________
# ****************************************************************************************************
# ****************                FUNCTION   PRCNTLRANK                    ***************************
# ****************                                                         ***************************
# ****************         creates a dataframe with perentle ranking       ***************************
# ****************************************************************************************************

prcntlRank<-function(finalstatsOut,Data4LastReading){
  
  #
  # WHAT IS NEEDED BY THIS FUNCTION?
  # finalstatsOut, Data4LastReading
  #
  # WHAT IS OUTPUT BY THIS FUNCTION? 
  # A dataframe with the result of the percentie ranking 
  # this includes the result of the ranking, the symbol color, RGB color 

  
  StatsForWeek <- finalstatsOut %>% filter(`Week of the year` == Data4LastReading$woy)
  
  if(is.na(Data4LastReading$value)){
    WLfreqResult<-"Missing last reading"
    SymbolCol<-"Gray"
    ColRGB<-"R191G191B191"
  }else{
    if(Data4LastReading$value < StatsForWeek$`10th percentile`){
      WLfreqResult<-"below the 10th percentile"
      SymbolCol<-"TuscanRed"
      ColRGB<-"R168G0B0"}
    if(Data4LastReading$value >= StatsForWeek$`10th percentile` & Data4LastReading$value < StatsForWeek$`25th percentile` ){
      WLfreqResult<-"within the 10th to 24th percentiles"
      SymbolCol<-"MarsRed"
      ColRGB<-"R255G0B0"}
    if(Data4LastReading$value >= StatsForWeek$`25th percentile` & Data4LastReading$value < StatsForWeek$`50th percentile` ){
      WLfreqResult<-"within the 25th to 49th percentiles"
      SymbolCol<-"MediumApple"
      ColRGB<-"R85G255B0"}
    if(Data4LastReading$value >= StatsForWeek$`50th percentile` & Data4LastReading$value <= StatsForWeek$`75 percentile` ){
      WLfreqResult<-"within the 50th to 75th percentiles"
      SymbolCol<-"MediumApple"
      ColRGB<-"R85G255B0"}
    
    if(Data4LastReading$value > StatsForWeek$`75 percentile`  & Data4LastReading$value <= StatsForWeek$`90th percentile` ){
      WLfreqResult<-"within the 76th to 90th percentiles"
      SymbolCol<-"SodaliteBlue"
      ColRGB<-"R190G232B255"}
    
    if(Data4LastReading$value > StatsForWeek$`90th percentile`){
      WLfreqResult<-"above the 90th percentile"
      SymbolCol<-"ApatiteBlue"
      ColRGB<-"R115G223B255"} 
    
  }
  
  prcntlRanking<-data.frame(cbind(WLfreqResult,SymbolCol,ColRGB))
  return(prcntlRanking)
}




# ____________________________________________________________________________________________________
# ****************************************************************************************************
# ****************                  FUNCTION FreqAnlysPlot                 ***************************
# ****************                                                         ***************************
# ****************               Makes frequency analysis plot             ***************************
# ****************************************************************************************************

FreqAnlysPlot <- function(lastYearofData, startDate, EndDate, path, variableInfo, siteName, siteNumber, quantData, lastYearofData_A, lastYearofData_P, yLimits, MonthInfo3, MonthInfo, statisticInfo, agencyCode){
  
  # Direct entry of variables for testing 
  # startDate<-computedDates$startDate
  # EndDate<-computedDates$EndDate
  
  # WHAT IS NEEDED BY THIS FUNCTION?
  # lastYearofData, startDate, EndDate, path, variableInfo, siteName, siteNumber,bp, quantData, lastYearofData_A, lastYearofData_P, yLimits, MonthInfo3, MonthInf
  #
  # WHAT IS OUTPUT BY THIS FUNCTION? 
  # Graphs of the frequency analysis sent to the plot directory given the "path" name.
  # 
  # ROOM FOR IMPROVEMENT
  # A lot of the stuff in EXPLANATION is hard coded. This needs to become variables.
  # It would be nice to not have to pass so many arugments to this function.
  #
  xlimDays<-c(0,nrow(lastYearofData))
  XTitle<-paste(format(as.Date(as.character(startDate)),format= "%B %Y"), 
                " to ", format(as.Date(as.character(EndDate)),format= "%B %Y"))
  
  png(file=path, width = 700, height = 600)
  par(oma = c(4, 1, 1, 1))
  par(mar = c(6, 5, 3, 1)+0.1)
  
  ylable<-paste("DAILY",toupper(statisticInfo),toupper(variableInfo$variableDescription))
  wr.lap <- wrap.labels(ylable, 60)
  fullID<-paste(agencyCode, siteNumber)
  
  
  bp<- barplot(stats, 
               xlab=XTitle, 
               ylab= wr.lap, 
               main=paste(c(siteName, fullID)),
               ylim=yLimits,xpd=FALSE,
               col=c("white", "palevioletred1","lightpink",
                     "darkolivegreen1","darkolivegreen1","lightcyan",
                     "lightblue2","white"),
               
               border = c("gray95","palevioletred2", "lightpink1",
                          "darkolivegreen2","darkolivegreen2","lightcyan2",
                          "lightblue3","gray95"),
               space=0,
               axes=TRUE,
               xaxs = "i",
               offset=0,
               tck=0.02)
  axis(4, labels=FALSE, tck=0.02)
  points(bp, quantData$'50th percentile', pch=17, cex=0.7, col="green")
  
  # ADDING LINES FOR APPROVED AND PROVISIONAL DATA TO PLOT 
  ptsx_A<-c(1:nrow(lastYearofData_A))
  ptsy_A<-lastYearofData_A$value
  lines(ptsx_A/7, ptsy_A, xlim=xlimDays, ylim=yLimits, pch=16, lwd=2, col="black")
  ptsx_P<-c(1:nrow(lastYearofData_P))
  ptsy_P<-lastYearofData_P$value
  lines(ptsx_P/7, ptsy_P, xlim=xlimDays, ylim=yLimits, pch=16, lwd=2, col="red")
  # END ADDING LINES TO PLOT  
  
  box()
  bp<-barplot(MonthInfo3, width=as.numeric(MonthInfo[3,]), names.arg=MonthInfo[2,], xaxs = "i", ylim=yLimits, xpd=FALSE,  col=NA, border = "gray70", space=0, add=TRUE)
  
  box()
  
  # Plot the approved data
  
  
  
  # Add notes to plots
  mtext("U.S. Geological Survey", adj=0, col="dark green")
  mtext(DateGraphText, side=3, adj=1,col="black", cex=0.8)
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE, xpd=TRUE) # this sets the legend up
  
  legend("bottomleft", 
         c("EXPLANATION", "   Approved daily data","   Provisional daily data", "   Median of historic daily data", "   Percentile classes (shown separately)", "   are compiled by week of year"), 
         lty=c(NA,1,1, NA, NA, NA), 
         pch=c(NA,NA,NA, 17, NA, NA),
         col=c("black", "black", "red", "green", "black", "black"),
         merge=FALSE,
         xpd = TRUE, 
         horiz = FALSE, 
         inset = c(0, -0.22)
  )
  
  
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE, xpd=TRUE) # this sets the legend up
  legend("bottomright", 
         c("Percentile classes", "   > 90", "   76-90","   25-75", "   10-24", "   < 10"), 
         pch=c(NA,15,15,15, 15, 15),
         col=c(NA, "lightblue2", "lightcyan", "darkolivegreen1", "lightpink", "palevioletred1"),
         merge=FALSE,
         xpd = TRUE, 
         horiz = FALSE, 
         inset = c(0, -0.22)
  )
  
  dev.off()
  # ----------------------- END FREQUECY ANALYSIS PLOT -------------------------------------------------
  
  return(xlimDays)
}
  


# ____________________________________________________________________________________________________
# ****************************************************************************************************
# ****************                  FUNCTION Make2yrPlot                   ***************************
# ****************                                                         ***************************
# ****************     MaKes 2-Year plot and associated text and table     ***************************
# ****************************************************************************************************

Make2yrPlot <- function(gwlevel, startDate2yr, EndDate, yLimits, startMonth, MainDir, FullSiteName, siteName, siteNumber, xlimDays,statisticInfo, agencyCode){
  
  
  # WHAT IS NEEDED BY THIS FUNCTION?
  # gwlevel, startDate2yr, EndDate, yLimits, startMonth, MainDir, FullSiteName, siteName, siteNumber
  # The "gwlevel" file is created outside this function, and has Date, Value, day of year (doy)
  # The start (startDate2yr) and end (EndDate) dates are currently computed outside this function as well as the "yLimits"
  
  # WHAT IS OUTPUT BY THIS FUNCTION? 
  # (1) Two year graphs which are sent to the "plots" folder under the Main Directory (MainDir)
  #     These graph files are named given the "FullSiteName", and include the "siteName" and "siteNumber" in the header
  # (2) The table providing the period of record day statistics (POYDailyStats) is also printed out by this function
  #
  # ROOM FOR IMPROVEMENT
  # (1) Agency is hard coded for the graph could bring in a variable for agency
  # (2) Text describing the variable in the EXPLANATION is hard coded as water level. This could be changed.
  
  # BEGIN 2-YEAR PLOT - create an empty data frame with just the dates for the full year, and merge 
  
  
  # 2-YEAR PLOT Get the last 2 years of data, divide the data into approved and provisional
  last2YearsofData<-gwlevel%>%
    filter(Date >= as.Date(startDate2yr))
  last2YearsofData_A<-last2YearsofData%>%
    filter(grepl("A", last2YearsofData$status))
  last2YearsofData_P<-last2YearsofData%>%
    filter(grepl("P", last2YearsofData$status))
  
  # Set up a column that preserves the orginal order of the file so that it can alway be put back in order.
  last2YearsofData$OrgOrder<-c(1:nrow(last2YearsofData))
  if(nrow(last2YearsofData_A) > 0){last2YearsofData_A$OrgOrder<-c(1:nrow(last2YearsofData_A))} 
  if(nrow(last2YearsofData_P) > 0){last2YearsofData_P$OrgOrder<-c(1:nrow(last2YearsofData_P))}
  
  
  
  # 2-YEAR PLOT create an empty data frame with just the dates for the last two full years and merge 
  # this with the last 2 years of data. This puts NAs where there is no data.
  full2Year<-as.data.frame(seq(as.Date(startDate2yr), as.Date(EndDate), by="days"))
  colnames(full2Year)<-"Date"
  
  last2YearsofData<-merge(full2Year, last2YearsofData, by="Date", all.x=TRUE)
  last2YearsofData_A<-merge(full2Year, last2YearsofData_A, by="Date", all.x=TRUE)
  last2YearsofData_P<-merge(full2Year, last2YearsofData_P, by="Date", all.x=TRUE)
  
  last2YearsofData$OrgOrder<-c(1:nrow(last2YearsofData))
  last2YearsofData_A$OrgOrder<-c(1:nrow(last2YearsofData_A))
  if(nrow(last2YearsofData_P) > 0){last2YearsofData_P$OrgOrder<-c(1:nrow(last2YearsofData_P))}
  
  # 2- YEAR PLOT - Compute daily water level statistics
  POYDailyStats<-gwlevel %>%
    group_by(doy)%>%
    summarize(
      max(value),
      mean(value),
      min(value),
      nDays = length(doy)
    )
  
  # 2 - YEAR PLOT - rearrange POYDAILYStats to be in same order as Last TWO YEARS of DATA, than merge
  # Compute day of year for the last 2 years and the original order
  full2Year$doy <- as.POSIXlt(full2Year$Date)$yday + 1
  full2Year$OrgOrder<-c(1:nrow(full2Year))
  # Merge the Period of Record Stats with this file. 
  POYDailyStats2yr<-merge(full2Year, POYDailyStats, 
                          by="doy", all.x=TRUE)
  # Reorder the file by the original order, this puts the stats in the same orderas the water level data 
  POYDailyStats2yr<-POYDailyStats2yr[order(POYDailyStats2yr[,3]),]
  # Merge the water level data and stats by date and get rid of excess columns
  POYStatsAndData<-as.data.frame(merge(POYDailyStats2yr[,c(2,4:7)], last2YearsofData[,c(1,4,12)], 
                                       by="Date", all.x=TRUE))
  
  # select the minimum daily value for each day of the year. This is where the bar showing the data will begin.
  Minimum<-POYStatsAndData[,4]
  # compute the difference between the max for each day of the year and the min. 
  # This is the length of the bar showing the data
  MinMax<-POYStatsAndData[,2]-POYStatsAndData[,4]
  # Take the results turn it into a dataframe, convert it to a matrix, and then transpose it.This is the way
  # that the bar plot will take the data.
  TwoYearStats<-as.data.frame(cbind(Minimum,MinMax))
  TwoYearDailyMinMax<-TwoYearStats
  TwoYearStats<-t(as.matrix(TwoYearStats))
  ylim2yr<-yLimits
  xlim2yr<-c(0,length(POYStatsAndData[,4]))
  
  # Create a matrix with the information for the month bar chart
  
  MonthInfo2yr<-rbind(
    rep(ylim2yr[2], 24),
    month.abb[rep(1:12,2)],
    rep(c(31,28.25,31, 30, 31, 30, 31, 31, 30, 31, 30, 31),2),
    rep(c("J","F", "M", "A", "M", "J", "J", "A", "S", "0", "N", "D"),2)
  )
  
  
  if(ylim2yr[1]<0){BotofBars<-(ylim2yr[2]-ylim2yr[1])*-1}else{BotofBars<-ylim2yr[1]}
  
  # rearrange the month info to be in same order as the data and stats for the last year
  if(startMonth != 1){
    monthstoplot <- c((startMonth):ncol(MonthInfo2yr),1:startMonth-1)
  } else {
    monthstoplot <- 1:ncol(MonthInfo2yr)
  }
  
  MonthInfo2yr<-MonthInfo2yr[,monthstoplot]
  
  path2yr <- paste(MainDir, "plots/", FullSiteName, "_", "2yr", ".png", sep='')
  XTitle2<-paste(format(as.Date(as.character(startDate2yr)),format= "%B %Y"), 
                 " to ", format(as.Date(as.character(EndDate)),format= "%B %Y"))
  
  MonthInfo2yr_b<-as.matrix(rbind(
    rep(ylim2yr[2], 24),
    rep(BotofBars, 24)
  ))
  
  MonthInfo2yrWidths<-as.numeric(MonthInfo2yr[3,])
  
  MonthInfo2yrNames<-MonthInfo2yr[4,]
  
  png(file=path2yr, width = 700, height = 600)
  
  par(oma = c(4, 1, 1, 1))
  par(mar = c(6, 5, 3, 1)+0.1)
  
  ylable<-paste("DAILY",toupper(statisticInfo),toupper(variableInfo$variableDescription))
  wr.lap <- wrap.labels(ylable, 60)
  fullID<-paste(agencyCode, siteNumber)
  
  bp_2YR<- barplot(TwoYearStats, 
                   xlab=XTitle2, 
                   ylab = wr.lap, 
                   main=paste(c(siteName, fullID)),
                   ylim=ylim2yr,xpd=FALSE,
                   xaxs = "i",
                   col=c("white","cyan"),
                   border = c("white","NA"),
                   space=0,
                   axes=TRUE,
                   offset=0,
                   tck=0.02)
  axis(4, labels=FALSE, tck=0.02)
  
  points(bp_2YR, POYStatsAndData[,3], pch=17, cex=0.5, col="yellow")
  
  bp_2yr<-barplot(MonthInfo2yr_b, width = MonthInfo2yrWidths, xaxs = "i", beside=FALSE, ylim=ylim2yr, xlim=xlimDays, xpd=FALSE, col=c(rep(NA,24)), border = c(rep("gray", 24)),space=c(rep(0,24)),las=3, add=TRUE, names.arg=MonthInfo2yrNames, cex.names=0.8)
  
  par(new=TRUE)
  x2yr<-c(1:length(last2YearsofData_A$value))
  y2yr<-last2YearsofData_A$value
  plot(last2YearsofData_A$value,
       ylim=ylim2yr,  axes=FALSE,xaxs = "i", xlab="",ylab="", lty=1, pch=19, cex=0.1, col="black")
  lines(x2yr, y2yr, xlim=xlim2yr, ylim=ylim2yr, pch=16, lwd=2, col="black")
  
  par(new=TRUE)
  x2yr<-c(1:length(last2YearsofData_P$value))
  y2yr<-last2YearsofData_P$value
  plot(last2YearsofData_P$value,
       ylim=ylim2yr,  axes=FALSE, xaxs = "i", xlab="",ylab="", lty=1, pch=19, cex=0.1, col="red")
  lines(x2yr, y2yr, xlim=xlim2yr, ylim=ylim2yr, pch=16, lwd=2, col="red")
  
  box()
  mtext("U.S. Geological Survey", adj=0, col="dark green")
  mtext(DateGraphText, side=3, adj=1,col="black", cex=0.8)
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE, xpd=TRUE) # this sets the legend up
  legend("bottomright", 
         c("EXPLANATION", "   Historic range in daily data", "   Approved daily data","   Provisional daily data", "    Mean of historic daily data"), 
         lty=c(NA,NA,1,1, NA), 
         pch=c(NA,15,NA,NA, 17),
         col=c("black", "cyan", "black", "red", "yellow"),
         merge=FALSE,
         xpd = TRUE, 
         horiz = FALSE, 
         inset = c(0, -0.22)
  )
  
  dev.off()
  
  # Daily statistics table
  TablepathDS <- paste0(MainDir,"tables/", FullSiteName, "_DS", ".html")
  names(POYDailyStats)<-c("Day of year","Maximum", "Mean", "Minimum", "Available data points for this day")
  POYDailyStats<-within(POYDailyStats, Mean<-round(POYDailyStats$Mean, digits = 2))
  print(xtable(POYDailyStats), type="html", file=TablepathDS, include.rownames=FALSE)
  
   # ----------------------- END Make 2-YEAR PLOT ----------------------------------------------------------- 
  return(POYStatsAndData)
}

# ____________________________________________________________________________________________________
# ****************************************************************************************************
# ****************                  FUNCTION MakeLTPlot                    ***************************
# ****************                                                         ***************************
# ****************     MaKes long term plot and associated text and table  ***************************
# ****************************************************************************************************

MakeLTPlot <- function(gwlevel, MainDir, FullSiteName, siteName, siteNumber, TrendTestResults, KTT_5Yr_TrendResults, KTT_20Yr_TrendResults, dateLastReading, statisticInfo, agencyCode){
  
  
  # WHAT IS NEEDED BY THIS FUNCTION?
  # gwlevel, MainDir, FullSiteName, siteName, siteNumber
  # The "gwlevel" file is created outside this function, and has Date, Value, day of year (doy)
 
  
  # WHAT IS OUTPUT BY THIS FUNCTION? 
  # (1) Longterm graphs which are sent to the "plots" folder under the Main Directory (MainDir)
  #     These graph files are named given the "FullSiteName", and include the "siteName" and "siteNumber" in the header
  # (2) The table providing the period of record day statistics (POYDailyStats) is also printed out by this function
  #
  # ROOM FOR IMPROVEMENT
  # (1) Agency is hard coded for the graph. Could bring in a variable for agency
  # (2) Text describing the variable in the EXPLANATION is hard coded as water level. This could be changed.
  
  #
  # Determine the maximums and minimuns of the continous and instantaneous data and determine plot limits. First make sure there are no NAs
  ContWLData<-na.omit(gwlevel$value)
  maxCont<-max(ContWLData)
  minCont<-min(ContWLData)
  ymaxLT<-maxCont
  yminLT<-minCont
  ylimLT<-c(yminLT, ymaxLT)
  
  
  LTpath <- paste(MainDir, "plots/", FullSiteName, "_", "LT", ".png", sep='')
  
  # The following block of code selects the approved data
  
  LTPlotData_A<-gwlevel%>%
    select(value, Date, status)%>%
    filter(grepl("A", gwlevel$status))
  
  # The following block of code selects the provisional data
  
  LTPlotData_P<-gwlevel%>%
    select(value, Date, status)%>%
    filter(grepl("P", gwlevel$status))
  
  
  
  #Check to see which starts first provisional or approved data. Determine the earliest start date .
  if (nrow(LTPlotData_A)==0){plotStartDate<-LTPlotData_P[1,2]} else{plotStartDate<-LTPlotData_A[1,2]} 
  
  #Check to see which ends last Provisional or Approved data. Determine the latest end date .
  if (nrow(LTPlotData_P)==0){plotEndDate<-LTPlotData_A[length(LTPlotData_A[,2]),2]} else{plotEndDate<-LTPlotData_P[length(LTPlotData_P[,2]),2]} 
  
  # Determine the limits for the long term plot
  
  xlimLT <-c(as.integer(plotStartDate), as.integer(plotEndDate))
  
  # The purpose of the following code is to add NA value for days that are missing so that 
  # when ploted there is a break in the line where the data are missing
  
  if (nrow(LTPlotData_A)>0){
    TimeDiff<-as.integer(LTPlotData_A[length(LTPlotData_A[,2]),2]-LTPlotData_A[1,2])
    FullTimeSeries <- seq(LTPlotData_A[1,2], by='1 day', length=TimeDiff)
    NAsAdded<-with(LTPlotData_A, value[match(FullTimeSeries, Date)]) # This is the tricky bit of code that does the magic
    # The above line matches the full time series to the time series that has gaps and adds NA where there is not a value.
    LTPlotData_A_NA<-data.frame(FullTimeSeries, NAsAdded)
    names(LTPlotData_A_NA)<-c("Date", "value")
  } 
  
  
  
  # In some instances the instantaneous data starts before the continous data, and this instantaneous data was not showing up in
  # in the long term plot. The following block of code tests to see if the instantaneous data started first.
  
  
  # The purpose of the following code is to add NA value for days that are missing so that 
  # when ploted there is a break in the line where the data are missing
  
  TimeDiff_P<-as.integer(LTPlotData_P[length(LTPlotData_P[,2]),2]-plotStartDate)
  if(length(LTPlotData_P$value) > 1){
    FullTimeSeries_P <- seq(plotStartDate, by='1 day', length=TimeDiff_P)
    NAsAdded_P<-with(LTPlotData_P, value[match(FullTimeSeries_P, Date)])
    LTPlotData_P_NA<-data.frame(FullTimeSeries_P, NAsAdded_P)
    
  } else {LTPlotData_P_NA<-LTPlotData_P}
  
  names(LTPlotData_P_NA)<-c("Date", "value")
  
  png(file=LTpath, width = 700, height = 600)
  par(oma = c(4, 1, 1, 1))
  par(mar = c(6, 5, 3, 1)+0.1)
 
   ylable<-paste("DAILY",toupper(statisticInfo),toupper(variableInfo$variableDescription))
  wr.lap <- wrap.labels(ylable, 60)
  fullID<-paste(agencyCode, siteNumber)
  # New plot code with x and y limits
  plot(gwlevel$Date,gwlevel$value, type = "l", col="white", ylim = ylimLT, xlim = xlimLT, main=paste(c(siteName, fullID)),
       xlab = "Date", ylab = wr.lap, tck=0.05)
  if(nrow(LTPlotData_A)>0){lines(LTPlotData_A_NA$Date,LTPlotData_A_NA$value, type = "l", col="blue")}
  if(nrow(LTPlotData_P)>0){lines(LTPlotData_P_NA$Date,LTPlotData_P_NA$value, type = "l", col="red")}
  
  
  # Check to see if irregularly spaced data (such as 5 days data) exists and if so plot it as points 
  
  if(nrow(LTPlotData_A)>0){
    LTPlotData_A_NA_new <- within(LTPlotData_A_NA, testNa <- (!is.na(LTPlotData_A_NA$value)))
    for(i in 1:length(LTPlotData_A_NA_new$value)){
      if(!is.na(LTPlotData_A_NA_new$value[i]) && is.na(LTPlotData_A_NA$value[i+1]) && is.na(LTPlotData_A_NA$value[i+2]) && is.na(LTPlotData_A_NA$value[i+3]) && is.na(LTPlotData_A_NA$value[i+4]) ){LTPlotData_A_NA_new$testNa[i]<-TRUE}else{LTPlotData_A_NA_new$testNa[i]<-FALSE}
    }
    LTPlotData_A_NA_new <-LTPlotData_A_NA_new %>% filter(testNa == TRUE)
    if(length(LTPlotData_A_NA_new$value) != 0){
      points(LTPlotData_A_NA_new$Date,LTPlotData_A_NA_new$value, pch=20, cex = .1, col="blue")
    }
  }
  
  # Add trend lines to the graph if the trends are statistically significant
  if(TrendTestResults$trend[1] == 'Up' || TrendTestResults$trend[1] == 'Down'){
    FiveyrLineStartDate<-as.numeric(as.Date(as.character(dateLastReading)))-5*365.25
    FiveyrLineEndDate<-as.numeric(as.Date(as.character(dateLastReading)))
    FiveyrFirstValue<-FiveyrLineStartDate *(KTT_5Yr_TrendResults$estimate['slope'])+ KTT_5Yr_TrendResults$estimate['intercept']
    FiveyrEndValue<-FiveyrLineEndDate *(KTT_5Yr_TrendResults$estimate['slope'])+ KTT_5Yr_TrendResults$estimate['intercept']
    segments(x0 = FiveyrLineStartDate, x1= FiveyrLineEndDate, y0 = FiveyrFirstValue, y1 = FiveyrEndValue, col="forestgreen", lty=5, lwd = 2)
  }
  
  if(TrendTestResults$trend[2] == 'Up' || TrendTestResults$trend[2] == 'Down'){
    TwentyyrLineStartDate<-as.numeric(as.Date(as.character(dateLastReading)))-20*365.25
    TwentyyrLineEndDate<-as.numeric(as.Date(as.character(dateLastReading)))
    TwentyyrFirstValue<-TwentyyrLineStartDate *(KTT_20Yr_TrendResults$estimate['slope'])+ KTT_20Yr_TrendResults$estimate['intercept']
    TwentyyrEndValue<-TwentyyrLineEndDate *(KTT_20Yr_TrendResults$estimate['slope'])+ KTT_20Yr_TrendResults$estimate['intercept']
    segments(x0 = TwentyyrLineStartDate, x1= TwentyyrLineEndDate, y0 = TwentyyrFirstValue, y1 = TwentyyrEndValue, col="forestgreen", lty=5, lwd = 2)
  }    
  # finish up the graph
  axis(3, labels=FALSE, tck=0.025)
  axis(4, labels=FALSE, tck=0.025)
  mtext("U.S. Geological Survey", adj=0, col="dark green")
  mtext(DateGraphText, side=3, adj=1,col="black", cex=0.8)
  
  
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE, xpd=TRUE) # this sets the legend up
  legend("bottomright", 
         c("EXPLANATION", "   Approved daily data (dashed where intermittent)", "   Provisional daily data", "   5- and 20-year trend lines (shown if statiscally significant)"), 
         lty=c(NA,1,1,5), 
         pch=c(NA,NA,NA,NA),
         col=c("black", "blue", "red","forestgreen"),
         lwd=c(1,1,1,2),
         merge=FALSE,
         xpd = TRUE, 
         horiz = FALSE, 
         inset = c(0, -0.22)
  )
  
  dev.off()
  
  
  
  
  
  # ------------------------- END LONG TERM PLOT ------------------------------------------------------
  
  
}


# ____________________________________________________________________________________________________
# ****************************************************************************************************
# ****************                  FUNCTION MakeTables                    ***************************
# ****************                                                         ***************************
# ****************     MaKes long term plot and associated text and table  ***************************
# ****************************************************************************************************

MakeTables <- function(gwlevel, MainDir, FullSiteName, current_site_info, TrendTestResults){
  
  # ____________________________________________________________________________________________________
  # ****************************************************************************************************
  # ******************************* CREATE OUTPUT TABLES  **********************************************
  # ****************************************************************************************************
  
  # WHAT DOES THIS FUNCTION REQUIRE?
  # gwlevel, MainDir, FullSiteName,current_site_info, TrendTestResults 
  #
  # WHAT DOES THIS FUNCTION CREATE?
  # (1) The Daily value table for the current site output to "table" folder of the main directory (MainDir) 
  #     including the datum from the "current_site_info" table. 
  # (2) A table of the trend test results (TrendTestResults) output to that same directory.
  #
  # ROOM FOR IMPROVEMENT
  # The type of data is hard coded as "water level in feet" this could be changed to be parameter specific.
  
  # Daily Water Level Table 
  TablepathDV <- paste0(MainDir,"tables/", FullSiteName, "_DV", ".html")
  DVtable <- cbind(gwlevel[,c(2:4)], rep(current_site_info$alt_datum_cd, length(gwlevel[,1])))
  names(DVtable)<- c("USGS site identifier", "Date", "Water level in feet", "datum")
  DVtable$Date<-as.character(DVtable$Date)
  print(xtable(DVtable), type="html", file=TablepathDV, include.rownames=FALSE)
  
  TablepathLTT <- paste0(MainDir,"tables/", FullSiteName, "_LTTrend", ".html")
  row.names(TrendTestResults)<-c("Five-year trend test results", "Twenty-year trend test results")
  print(xtable(TrendTestResults), type="html", file=TablepathLTT, include.rownames=TRUE)
  
  # GENERATE TEXT FOR DAILY DATA SUMMARY
  
  
  dailyDataSummary<-as.matrix(summary(as.vector(gwlevel$value)))
  if(length(row.names(dailyDataSummary))==6){row.names(dailyDataSummary)<-c("Minimum", "1st quartile", "Median", "Mean", "3rd quartile", "Maximum")}
  if(length(row.names(dailyDataSummary))==7){row.names(dailyDataSummary)<-c("Minimum", "1st quartile", "Median", "Mean", "3rd quartile", "Maximum", "Number of missing values")}
  dailyDataSummary<-as.data.frame(t(dailyDataSummary))
  # CREATE A TABLE FOR THE DAILY DATA SUMMARY
  
  
  print(xtable(dailyDataSummary), type="html", file=paste0(MainDir,"tables/", FullSiteName,"dailyDataSummary", ".html"), include.rownames=FALSE)
  return(dailyDataSummary)
}

# ____________________________________________________________________________________________________
# ****************************************************************************************************
# ****************                  FUNCTION computeKTT                    ***************************
# ****************                                                         ***************************
# ****************         Compute the Kendal Trend Test Results           ***************************
# ****************************************************************************************************

computeKTT <- function(gwlevel, trendPeriod){
  
  # WHAT IS NEEDED BY THIS FUNCTION?
  # gwlevel, trendPeriod
  #
  # WHAT IS OUTPUT BY THIS FUNCTION? 
  # A data frame with the Season Kendal Trend Trend results for the specified periods
  # TrendTestResults
  
  
  ###############################################################################
  #                                                                             #
  ###################       Kendall Trend Test    ##############################
  #                                                                             #
  ###############################################################################
  
  # Compute monthy means 
  
  MonthlyMeans<-gwlevel %>%
    group_by(year,month)%>%
    summarize(
      mean(value, na.rm = TRUE),
      nDays = length(doy)
    )%>%
    rename("meanWL" = "mean(value, na.rm = TRUE)")%>%
    rename("ndays" = "nDays")
  # censor the means of any months that are based on less than 15 days of data
  MonthlyMeansCensored<-MonthlyMeans %>%
    filter(ndays>14)
  # determine how many years of data are available for analysis
  YearsOfRecord<-length(MonthlyMeansCensored$month)/12
  MonthlyMeansLast5Yrs<-MonthlyMeansCensored %>%
    filter(year>=(as.numeric(checkResults$currentYear)-trendPeriod))
  MonthlyMeansLast5Yrs<-within(MonthlyMeansLast5Yrs, dayN<-rep(15, length(year)))
  MonthlyMeansLast5Yrs<-within(MonthlyMeansLast5Yrs, NewDate<-as.Date(paste(year,month,dayN, sep="."), format = "%Y.%m.%d"))
  KTT_5Yr_TrendResults<-kendallTrendTest(y = MonthlyMeansLast5Yrs$meanWL ~ MonthlyMeansLast5Yrs$NewDate)
  
  return(KTT_5Yr_TrendResults)
  
}


# ____________________________________________________________________________________________________
# ****************************************************************************************************
# ****************                  FUNCTION MakeText                      ***************************
# ****************                                                         ***************************
# ****************       MaKes text blocks for station pages               ***************************
# ****************************************************************************************************

MakeText <- function(statisticInfo, current_site_info, POYStatsAndData, dateLastReading, siteNumber,siteName, ValueLastReading, WLfreqResult, Data4LastReading, dailyDataSummary,variableInfo, TypeCurrentSite, FullSiteName){
  # ____________________________________________________________________________________________________
  # ****************************************************************************************************
  # ******************************* CREATE OUTPUT TABLES  **********************************************
  # ****************************************************************************************************
  
  # WHAT DOES THIS FUNCTION REQUIRE?
  # statisticInfo, current_site_info, POYStatsAndData, dateLastReading, siteNumber,siteName, ValueLastReading, WLfreqResult, Data4LastReading, dailyDataSummary 
  #
  # WHAT DOES THIS FUNCTION CREATE?
  # This function creates the text for the station pages including:
  # (1) The site information header
  # (2) Text that goes next to the 2-yr plot
  # (3) Text that goes next to the frequency analysis plot summerizing the frequency analysis data.
  # (4) Text that goes next to the long term graph summerizing daily data. 
  #
  # ROOM FOR IMPROVEMENT
  # A lot of this text is hard coded and would need to be pulled from variables so that this coded would work with all kinds of of continuous data.
  # Much of this information was designed for groundwater sites and would need to be modified for surface water sites. 
  # 
  
  # _________________________________________________________________________________________________
  # *************************************************************************************  
  # **************************                                  ************************* 
  # **************************  GENERATE TEXT FOR STATION PAGES *************************
  # **************************                                  *************************
  # *************************************************************************************
  
  
  # GENERATE SITE INFORMATION TEXT
  # Generate the text describing statistic and datum
  
  
  # Based on the current site parse out the information to create the station summary which includes
  # Site ID, Site Name,  LSD, and site location info
  
  outputfilname<-paste0(FullSiteName,".txt")
  
  lt <- current_site_info$lat_va
  ln <- current_site_info$long_va
  hdatum<-current_site_info$coord_datum_cd
  latlongtxt <- sprintf("latitude %s %s'%s\", longitude %s %s'%s\" ",
                        substr(lt,1,2),substr(lt,3,4),substr(lt,5,6),
                        substr(ln,1,2),substr(ln,3,4),substr(ln,5,6))
  locationText1<-paste0("Location: ",latlongtxt, hdatum, ", landnet: ", current_site_info$land_net_ds)
  locationText2<-paste0("          ", countyCdLookup(current_site_info$state_cd, current_site_info$county_cd,  output = "fullName"),", ",stateCdLookup(current_site_info$state_cd, "fullName"),", hydrologic unit ", current_site_info$huc_cd)
  depthText<-paste0("Well depth: ", current_site_info$well_depth_va, " feet")
  altitudeText<-paste0("Land surface altitude: ", current_site_info$alt_va, " feet above ", current_site_info$alt_datum_cd)
  
  cat(locationText1)
  sink(paste0(MainDir,"HeaderFiles/",outputfilname))
  cat(locationText1)
  cat("\n")
  cat(locationText2)
  cat("\n")
  if(TypeCurrentSite == "GW"){
    cat(depthText)
    cat("\n")
    
  }
  cat(altitudeText)
  cat("\n")
  sink()
  
  
  # END GENERATING SITE INFORMATION TEST
  
  # GENERATE TEXT NEXT TO TWO YEAR PLOT
  StatsForDay<-POYStatsAndData%>% filter(Date == dateLastReading)
  names(StatsForDay)<-c("Date", "Maximum", "Mean", "Minimum", "Number_of_days", "Current Water Level","OrgOrder")
  Yr2Textname<-paste0(FullSiteName,"2yr.txt")
  # Generate the text
  Sentence1 <-paste0("Summary of records of ",tolower(statisticInfo$statisticName)," ", tolower(variableInfo$variableDescription))
  # Generate the text describing the value of the last reading
  Sentence2 <-paste0("The most recent daily value was recorded on ", dateLastReading, ", and is ", ValueLastReading," ", variableInfo$unit)
  Sentence3b <-paste0("The highest value ever recorded at this site ")
  Sentence4 <- paste0("on this day of the year was ", StatsForDay$Maximum," ",variableInfo$unit, " and the")
  Sentence5 <- paste0("lowest was ", StatsForDay$Minimum," ",variableInfo$unit)
  
  cat(Sentence1)
  sink(paste0(MainDir,"HeaderFiles/", Yr2Textname))
  cat(Sentence1)
  cat("\n")
  cat(Sentence2)
  cat("\n")
  cat(Sentence3b)
  cat("\n")
  cat(Sentence4)
  cat("\n")
  cat(Sentence5)
  cat("\n")
  sink()
  
  
  
  
  
  # GENERATE TEXT NEXT TO FREQUENCY ANALYSIS PLOT
  FreqTextname<-paste0(FullSiteName,"frq.txt")
  
  
  Sentence3 <-paste0("This value is ", WLfreqResult," for week ",Data4LastReading$woy," of the year.")
  
  cat(Sentence1)
  sink(paste0(MainDir,"HeaderFiles/", FreqTextname))
  cat(Sentence1)
  cat("\n")
  cat(Sentence2)
  cat("\n")
  cat(Sentence3)
  cat("\n")
  sink()
  
  # Generate the text that goes next to the long term plot
  DailyTextname<-paste0(FullSiteName,"Daily.txt")
  # Generate the text
  Sentence1b <-paste0("Summary statistics of ", tolower(statisticInfo$statisticName)," daily ", tolower(variableInfo$variableDescription))
  Sentence2b <- paste0(statisticInfo$statisticName," daily ", tolower(variableInfo$variableName), " is available for")
  Sentence3c <- paste0("the period ", min(gwlevel$Date)," to ", max(gwlevel$Date),".")
  Sentence4b <- paste0("During this period, ", tolower(variableInfo$variableName), " ranged from")
  Sentence5b <- paste0(round(dailyDataSummary$Minimum, digits = 2)," to ", round(dailyDataSummary$Maximum, digits = 2), " ", variableInfo$unit, ", and averaged ", round(dailyDataSummary$Mean, digits = 2), " ", variableInfo$unit)
  
  
  cat(Sentence1b)
  sink(paste0(MainDir,"HeaderFiles/", DailyTextname))
  cat(Sentence1b)
  cat("\n")
  cat(Sentence2b)
  cat("\n")
  cat(Sentence3c)
  cat("\n")
  cat(Sentence4b)
  cat("\n")
  cat(Sentence5b)
  cat("\n")
  sink()
  
}


# ____________________________________________________________________________________________________
# ****************************************************************************************************
# ****************                  FUNCTION MakeText2                      ***************************
# ****************                                                         ***************************
# ****************       MaKes text blocks for station pages               ***************************
# ****************************************************************************************************

MakeText2 <- function(statisticInfo, current_site_info,  dateLastReading, siteNumber,siteName, ValueLastReading,  Data4LastReading, dailyDataSummary, variableInfo, TypeCurrentSite, FullSiteName){
  # ____________________________________________________________________________________________________
  # ****************************************************************************************************
  # ******************************* CREATE OUTPUT TABLES  **********************************************
  # ****************************************************************************************************
  
  # WHAT DOES THIS FUNCTION REQUIRE?
  # statisticInfo, current_site_info,  dateLastReading, siteNumber,siteName, ValueLastReading,  Data4LastReading, dailyDataSummary 
  #
  # WHAT DOES THIS FUNCTION CREATE?
  # This function creates the text for the station pages including:
  # (1) The site information header
  # (2) Text that goes next to the 2-yr plot
  # (3) Text that goes next to the frequency analysis plot summerizing the frequency analysis data.
  # (4) Text that goes next to the long term graph summerizing daily data. 
  #
  # ROOM FOR IMPROVEMENT
  # A lot of this text is hard coded and would need to be pulled from variables so that this coded would work with all kinds of of continuous data.
  # Much of this information was designed for groundwater sites and would need to be modified for surface water sites. 
  # 
  
  # _________________________________________________________________________________________________
  # *************************************************************************************  
  # **************************                                  ************************* 
  # **************************  GENERATE TEXT FOR STATION PAGES *************************
  # **************************                                  *************************
  # *************************************************************************************
  
  
  # GENERATE SITE INFORMATION TEXT
  # Generate the text describing statistic and datum
  
  
  # Based on the current site parse out the information to create the station summary which includes
  # Site ID, Site Name,  LSD, and site location info
  
  outputfilname<-paste0(FullSiteName,".txt")
  
  lt <- current_site_info$lat_va
  ln <- current_site_info$long_va
  hdatum<-current_site_info$coord_datum_cd
  latlongtxt <- sprintf("latitude %s %s'%s\", longitude %s %s'%s\" ",
                        substr(lt,1,2),substr(lt,3,4),substr(lt,5,6),
                        substr(ln,1,2),substr(ln,3,4),substr(ln,5,6))
  locationText1<-paste0("Location: ",latlongtxt, hdatum, ", landnet: ", current_site_info$land_net_ds)
  locationText2<-paste0("          ", countyCdLookup(current_site_info$state_cd, current_site_info$county_cd,  output = "fullName"),", ",stateCdLookup(current_site_info$state_cd, "fullName"),", hydrologic unit ", current_site_info$huc_cd)
  depthText<-paste0("Well depth: ", current_site_info$well_depth_va, " feet")
  altitudeText<-paste0("Land surface altitude: ", current_site_info$alt_va, " feet above ", current_site_info$alt_datum_cd)
  
  cat(locationText1)
  sink(paste0(MainDir,"HeaderFiles/",outputfilname))
  cat(locationText1)
  cat("\n")
  cat(locationText2)
  cat("\n")
  if(TypeCurrentSite == "GW"){
    cat(depthText)
    cat("\n")
    
  }
  cat(altitudeText)
  cat("\n")
  sink()
  
  
  # END GENERATING SITE INFORMATION TEST
  
  
  
  # Generate the text that goes next to the long term plot
  DailyTextname<-paste0(siteNumber,siteName,"Daily.txt")
  # Generate the text
  Sentence1b <-paste0("Summary statistics of ", tolower(statisticInfo$statisticName)," daily ", tolower(variableInfo$variableDescription))
  Sentence2b <- paste0(statisticInfo$statisticName," daily ", tolower(variableInfo$variableName), " is available for")
  Sentence3c <- paste0("the period ", min(gwlevel$Date)," to ", max(gwlevel$Date),".")
  Sentence4b <- paste0("During this period, ", tolower(variableInfo$variableName), " ranged from")
  Sentence5b <- paste0(round(dailyDataSummary$Minimum, digits = 2)," to ", round(dailyDataSummary$Maximum, digits = 2), " ", variableInfo$unit, ", and averaged ", round(dailyDataSummary$Mean, digits = 2), " ", variableInfo$unit)
  
  
  cat(Sentence1b)
  sink(paste0(MainDir,"HeaderFiles/", DailyTextname))
  cat(Sentence1b)
  cat("\n")
  cat(Sentence2b)
  cat("\n")
  cat(Sentence3c)
  cat("\n")
  cat(Sentence4b)
  cat("\n")
  cat(Sentence5b)
  cat("\n")
  sink()
  
}
  
# ____________________________________________________________________________________________________
# ****************************************************************************************************
# ****************              FUNCTION numbers2words                     ***************************
# ****************                                                         ***************************
# ****************       changes numbers into the text equivalent          ***************************
# ****************************************************************************************************  
# This is a funtion created by someone else and found on the web
# Tony Hirst psychemedia

#https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
numbers2words <- function(x){
  ## Function by John Fox found here: 
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
  helper <- function(x){
    
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and", 
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],"," ,
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    #Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    #Clear any trailing " and"
    text=gsub(" and$","",text)
    #Clear any trailing comma
    gsub("\ *,$","",text)
  }  
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))     
  #Disable scientific notation
  opts <- options(scipen=100) 
  on.exit(options(opts)) 
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine") 
  names(ones) <- 0:9 
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9 
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety") 
  names(tens) <- 2:9 
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")     
  if (length(x) > 1) return(trim(sapply(x, helper)))
  helper(x)
}


# ____________________________________________________________________________________________________
# ****************************************************************************************************
# ****************              FUNCTION WRAP.IT                           ***************************
# ****************                                                         ***************************
# ****************        Wraps text for use in graphs                     ***************************
# ****************************************************************************************************  
# This is a funtion created by someone else and found on the web
# Marc Schwartz in his post to R-help:

# Core wrapping function
wrap.it <- function(x, len)
{ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = "\n"), 
         USE.NAMES = FALSE)
}

#  Call this function with a list or vector
wrap.labels <- function(x, len)
{
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}
