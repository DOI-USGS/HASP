

#TREND ANALYSIS FUNCTIONS
#
# Created by Lindsay Carr - 6/15/2016, rewritten by Scott Prinos 6/24/2016, Modified by Scott Prinos - 7/24/2018
# 
# Modification made: Pointed to markerLookupTable2 which does not specify fat or thin arrows for 5- or 20-year trends.
# and modified lines 322-361 so that the different symbols are still provided for the overall trend
# This modification has been made because the website will no longer show fat or thin arrows but
# will instead have radio buttons for so that 5 and 20 year trends can be viewed individually.
# Nonetheless I still wanted the overall result just in case we want to use it.

# These are fuctions that are called by the script ChlorideSpecCondAnalysisScript_mod5.R,
# this script relies on additional functions in the script helperfunction_mod5.R
#

#_______________________________________________________________________
# FUNCTION - trendAnalysis
#************************************************************************
# This is the main chloride trend analysis script, but it is very modular in design
# it calls a series of other funtions that conduct parts of the analysis, including:
# - DataCompletnessAnalyis
# - getSalinityColor
# - trendTest
# - OverallTrend
#
#
#
# **********************************************************************

trendAnalysis <- function(siteID, stationName, clData_raw, pctComplete){
  
  


# Modify the input data from the site into the necessary date form for analysis 
  clData <- clData_raw %>% 
    rename(Chloride = result_va) %>%
    mutate(Date = sample_dt) %>%  
    mutate(Year = as.numeric(format(Date, "%y")),
           Month = as.numeric(format(Date, "%m"))) %>% 
    select(Date, Year, Month, Chloride)
 
 # Compute dates of the beginning and ends of the 5 and 20 year trend analyses    
  mostRecentDate <- as.POSIXlt(max(clData$Date, na.rm = TRUE))
  beginDate_five <- mostRecentDate
  beginDate_twenty <- mostRecentDate
  beginDate_five$year <- mostRecentDate$year - 5
  beginDate_twenty$year <- mostRecentDate$year - 20

# Pull data for the 5 and 20 year trend analyses and put them in seperate variables
  clData_five <- clData %>% 
    filter(Date >= as.Date(beginDate_five))
  clData_twenty <- clData %>% 
    filter(Date >= as.Date(beginDate_twenty))
  # CALL FUNCTION DataCompletnessAnalyis
  DataCmpltAnaly<-DataCompletnessAnalyis(clData_five, clData_twenty, pctComplete)
  # CALL FUNCTION getSalinityColor
  salinityCol <- getSalinityColor(tail(clData$Chloride,1)) 
  # CALL FUNCTION trendTest
  cltrend_five <- trendTest(trendType = "Five Year", salinityCol = salinityCol, 
                            clData = clData_five, pctComplete = pctComplete, MainDir = MainDir, DataCmpltAnaly=DataCmpltAnaly)
  cltrend_twenty <- trendTest(trendType = "Twenty Year", salinityCol = salinityCol, 
                              clData = clData_twenty, pctComplete = pctComplete, MainDir = MainDir, DataCmpltAnaly=DataCmpltAnaly)
  # CALL FUNCTION OverallTrend
  cltrend_all <- OverallTrend(salinityCol, cltrend_five, cltrend_twenty)
  # Create files with the tresults of the trend analyses
  cltrend <- bind_rows(cltrend_five, cltrend_twenty, cltrend_all)
  chloride = clData$Chloride[which(clData$Date == as.Date(mostRecentDate))]
  if (length(chloride)>1){chloride<-chloride[1]}
  trendResults <- data.frame(siteID = siteID, 
                             stationName = stationName, 
                             lastSampleDate = as.Date(mostRecentDate), 
                             chloride = chloride,
                             trendType = c("Five Year", "Twenty Year", "Overall"),
                             stringsAsFactors = FALSE)
  trendResults <- left_join(trendResults, cltrend)
  
# Send the trend results back to the main script.
  return(trendResults)
}

# END FUNCTION - trendAnalysis
#_________________________________________________________________


#_________________________________________________________________
# FUNCTION - trendTest
# ****************************************************************
#
# This trend test funciton calls the kendal trend test function developed by the EPA. The season kendal trend
# trend could also have been used but much of the data is annual and there is limited seasonality to this data. 
# In most instances the overwhelming feature is inland movement of the front rather than seasonality. 
#
#******************************************************************

trendTest <- function(trendType, salinityCol, clData = NULL, pctComplete = NULL, 
                      cltrend_five = NULL, cltrend_twenty = NULL, MainDir, DataCmpltAnaly){
  
# *************************************** FOR TESTING REMOVE WHEN DONE ******************************
#  
#  trendType = "Five Year"
#  salinityCol = salinityCol
#  clData = clData_five
#  pctComplete = pctComplete
#  MainDir = MainDir
#  DataCmpltAnaly=DataCmpltAnaly
#  
# ***************************************************************************************************
  
  #trend calc for five and twenty
  if(trendType != "Overall"){
    
    if(trendType=="Five Year"){
      trendLength <- 5
      enoughData <- as.logical(DataCmpltAnaly$enoughData5)
    }else{
      trendLength<-20
      enoughData<-as.logical(DataCmpltAnaly$enoughData20)
    }
    
    # only calculate trend when there is enough data
    if(enoughData){
     
      # CALL FUNCTION trendTest_Kendall
      trendtest <- trendTest_Kendall(clData) 
      # CALL FUNCTION interpretTrend
      trend_results <- interpretTrend(trendtest, trendLength, trendType)
    } else {
      trend_results <- data.frame(trendType = trendType,
                                  trend = "Insufficient data",
                                  stringsAsFactors = FALSE)
    } 
  } 
  
  # add color marker
  # left_join with template so that all columns are present
  # NAs remain in columns if trend_results does not have that column
  # CALL FUNCTION markerLookup
  trend_results <- trend_results %>% 
    mutate(colorMarker = markerLookup(trendType, trend_results$trend, salinityCol, MainDir)) %>% 
    left_join(data.frame(trendType = trendType, trend = as.character(NA), colorMarker = as.character(NA), 
                         pValue = as.numeric(NA), tau = as.numeric(NA), slope = as.numeric(NA), 
                         intercept = as.numeric(NA), rlwd = as.numeric(NA), stringsAsFactors = FALSE))
  return(trend_results)
}
# END FUNCTION - trendTest
#_________________________________________________________________



#_________________________________________________________________
# FUNCTION - interpretTrend
# ****************************************************************
#******************************************************************
interpretTrend <- function(clTrend, trendLength, trendType){
  slope <- NA
  intercept <- NA
  rlwd <- NA
  tau <- clTrend$tau
  pValue <- clTrend$pValue ### OR USE Chi-Square (Het) ???
  
  # is there a trend? 
  # is it statistically significant? 
  # is it positive or negative?
  if(tau == 0){ 
    trend <- "None"
  } else if(pValue >= 0.05){ 
    trend <- "Not significant"
  } else { 
    slope <- clTrend$slope
    intercept <- clTrend$intercept
    rlwd <- ifelse(trendLength == 5, 2, 3)
    trend <- ifelse(slope > 0, "Up", "Down")
  }
  
  return(data.frame(trendType, trend, pValue, tau, slope, intercept, rlwd,
                    stringsAsFactors = FALSE))
}
#END FUNCTION interpretTrend
#_________________________________________________________________

#_________________________________________________________________
# FUNCTION - markerLookup
# ****************************************************************
#******************************************************************

markerLookup <- function(cl_trendType, cl_trend, salinityCol, MainDir){
  markerTable <- read.csv(paste(MainDir,'InputFiles/markerLookupTable2.csv', sep = ""), 
                          stringsAsFactors = FALSE)
  
  markerLookupSubset <- markerTable %>% 
    filter(trendType == cl_trendType) %>% 
    filter(trend == cl_trend)
  
  markerDescription <- markerLookupSubset$markerDescription
  color_markerDescription <- paste(salinityCol, markerDescription, sep="-")
  
  return(color_markerDescription)
}
# END FUNCTION - markerLookup
#________________________________________________________________



#_________________________________________________________________
# FUNCTION - getSalinityColor
# ****************************************************************
#******************************************************************

getSalinityColor <- function(cl){
  
  #salinity category
  if(cl < 250){
    salinity_color <- "LightBlue"
  } else if(cl >= 250 && cl < 1000){
    salinity_color <- "Orange"
  } else if(cl >= 1000){
    salinity_color <- "Red"
  }
  
  return(salinity_color)
}

# END FUNCTION - getSalinityColor
#__________________________________________________________________



#_________________________________________________________________
# FUNCTION - trendTest_seaonalKendall
# ****************************************************************
# 
# Function not currently used, but it could be used to replace the 
# kendall trend test function. 
#
# ****************************************************************

trendTest_seasonalKendall <- function(clData){
  trend_results <- kendallSeasonalTrendTest(data = clData, y = Chloride ~ Month + Year)
  results <- list(tau = trend_results$estimate['tau'],
                  pValue = trend_results$p.value['z (Trend)'],
                  slope = trend_results$estimate['slope'],
                  intercept = trend_results$estimate['intercept'],
                  trendObject = trend_results)
  return(results)
}

# END FUNCTION - trendTest_seaonalKendall
#_________________________________________________________________


#_________________________________________________________________
# FUNCTION - trendTest_Kendall
# ****************************************************************

trendTest_Kendall <- function(clData){
  
  
  clData <- clData %>% mutate(Date_num = as.numeric(Date))
  trend_results <- kendallTrendTest(y = clData$Chloride ~ clData$Date_num)
  
  # trend_results <- kendallTrendTest(y = clData$Chloride ~ clData$Date) # This is the old code for the line above
  results <- list(tau = trend_results$estimate['tau'],
                  pValue = trend_results$p.value['z'],
                  slope = trend_results$estimate['slope'],
                  intercept = trend_results$estimate['intercept'],
                  trendObject = trend_results)
  return(results)
}

# END OF FUNCTION - trendTest_Kendall
#________________________________________________________________


#_________________________________________________________________
# FUNCTION - DataCompletnessAnalyis
# ****************************************************************
# Test to see if there is enough data for the 5
# and 20 year trend analyses
#
# ****************************************************************
DataCompletnessAnalyis <- function(clData_five, clData_twenty, pctComplete){
  NoOfObs20<-length(clData_twenty$Year)
  NoOfYears20<-length(unique(clData_twenty$Year))
  AvgObsPerYr20<-NoOfObs20/NoOfYears20
  NoOfObs5<-length(clData_five$Year)
  NoOfYears5<-length(unique(clData_five$Year))
  AvgObsPerYr5<-NoOfObs5/NoOfYears5
  if(NoOfYears5>(5*pctComplete)){
    enoughData5<-"TRUE"
  } else{enoughData5 <- "FALSE"}
  if(NoOfYears20>(20*pctComplete)){
    enoughData20<-"TRUE"
  } else{enoughData20 <- "FALSE"}
  DataCmpltAnaly<-data.frame(cbind(NoOfObs5, NoOfYears5, AvgObsPerYr5, enoughData5, NoOfObs20,NoOfYears20,AvgObsPerYr20,enoughData20))
  return(DataCmpltAnaly)
}

# END FUNCTION - DataCompletnessAnalyis
#________________________________________________________________



#_________________________________________________________________
# FUNCTION - OverallTrend
# ****************************************************************
#
# Determine what the overall trend shown on the map is going to be.
#
# If there is a statistically significant trend for one of the periods
# and no trend or not enough data to test for the other periods than the 
# trend for the available period is shown.
#
# If the trend direction is the same for both periods than the 20 year trend 
# symbol is shown.
#
# If the 5 and 20 year trends are in different directions than the double headed 
# arrow is shown.
# 
#
#******************************************************************

OverallTrend <- function(salinityCol, cltrend_five, cltrend_twenty){
  if(cltrend_twenty$trend ==  cltrend_five$trend){
    if(cltrend_twenty$trend == "Up"){Marker<-"FatUpArrow"
      colorMarker<-paste(salinityCol, Marker, sep="-")}
    if(cltrend_twenty$trend == "Down"){Marker<-"FatDownArrow"
      colorMarker<-paste(salinityCol, Marker, sep="-")}
    if(cltrend_twenty$trend == "Insufficient data"){Marker<-"Square"
      colorMarker<-paste(salinityCol, Marker, sep="-")}
    if(cltrend_twenty$trend == "Not significant"){Marker<-"Circle"
      colorMarker<-paste(salinityCol, Marker, sep="-")}
    trend<-cltrend_twenty$trend
  } else if((cltrend_five$trend == "Up" && cltrend_twenty$trend == "Down") 
            |(cltrend_five$trend == "Down" && cltrend_twenty$trend == "Up")){
    Marker<-"DoubleHeadedArrow"
    colorMarker<-paste(salinityCol, Marker, sep="-")
    trend<-"Opposite"
  } else if((cltrend_five$trend == "Up" | cltrend_five$trend == "Down") 
            && (cltrend_twenty$trend != "Up" | cltrend_twenty$trend != "Down")){
      if(cltrend_five$trend == "Up"){Marker<-"ThinUpArrow"
      colorMarker<-paste(salinityCol, Marker, sep="-")}
      if(cltrend_five$trend == "Down"){Marker<-"ThinDownArrow"
      colorMarker<-paste(salinityCol, Marker, sep="-")}
      if(cltrend_five$trend == "Insufficient data"){Marker<-"Square"
      colorMarker<-paste(salinityCol, Marker, sep="-")}
      if(cltrend_five$trend == "Not significant"){Marker<-"Circle"
      colorMarker<-paste(salinityCol, Marker, sep="-")}
    trend<-cltrend_five$trend
  } else if((cltrend_twenty$trend == "Up" | cltrend_twenty$trend == "Down") 
           && (cltrend_five$trend != "Up" | cltrend_five$trend != "Down")){
          if(cltrend_twenty$trend == "Up"){Marker<-"FatUpArrow"
            colorMarker<-paste(salinityCol, Marker, sep="-")}
          if(cltrend_twenty$trend == "Down"){Marker<-"FatDownArrow"
            colorMarker<-paste(salinityCol, Marker, sep="-")}
      trend<-cltrend_twenty$trend
  } else {
    Marker<-"Circle"
    colorMarker<-paste(salinityCol, Marker, sep="-")
    trend<-"Indeterminate"
  }
 
  cltrend_overall<-data.frame(trendType = "Overall", trend, colorMarker, 
             pValue = as.numeric(NA), tau = as.numeric(NA), slope = as.numeric(NA), 
             intercept = as.numeric(NA), rlwd = as.numeric(NA), stringsAsFactors = FALSE)
  return(cltrend_overall)
  
  }
    
# END FUNCTION - OverallTrend
#________________________________________________________________

