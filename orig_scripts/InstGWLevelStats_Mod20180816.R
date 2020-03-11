# ********************************************************************************************************
# R SCRIPT                                                                                               *
# InstGWLevelStats_Mod20180816                                                                            *
#                                                                                                        *
# Based on script by initially developed by Laura DeCicco, USGS Center for Integrated Data Analytics     *
# Modified by Scott Prinos, Hydrologist, USGS Carebean-Florida Water Science Center, Davie Florida       *
# August 16, 2018                                                                                         *
#                                                                                                        *
#*********************************************************************************************************

# Modifications made to code. Called measurements manual instead of periodic. Added code to provide month
# names in the output tables instead of month numbers. Added code to indcate the datum to which the water
# the water levels are referenced. Added code to indicate which month's frequency analysis the last value
# is compared to.


# The following information is for set up only and does not need to be rerun each time
# It can be commented out or removed
# 


library(dataRetrieval)
library(xtable)
#library(xlsx)
library(dplyr)
library(EnvStats)
library(lubridate)
library(openxlsx)






# Change "MainDir" to whereever you want the output to go. Create a folder with that main directory 
# as well as subdirectories for the input files, plots, and tables.

#___________________________________________________________________________________________________
# **************************************************************************************************
# *                                                                                                *
# *                                                                                                *
# *                              START OF MAIN PROGRAM                                             *
# *                                                                                                *
# *                                                                                                *
# **************************************************************************************************
#___________________________________________________________________________________________________

# ___________________________________________________________________________________________
# INITIAL SET UP 
#
# This section pulls the intial data and siting listing
#
MainDir <- "C:/Users/stprinos/Documents/stprinos_data/NewWebsite/" # Path for the main directory

TableWLChanges <-0 #  The variable TableWaterLevel will be in the que from the previous code runs and we do not want that. But a simple removal would throw an error code on the first code run. 
rm(TableWLChanges) #  This removes TableWLChanges and will not throw an error code, since the variable does exist given the previous line.


#_________________________________________________________________
# FUNCTION - markerLookup
# ****************************************************************
#******************************************************************

markerLookup <- function(inputTrend){
 
  markerTable <- read.csv(paste(MainDir,'InputFiles/GenericMarkerLookupTable.csv', sep = ""), 
                          stringsAsFactors = FALSE)
  
  markerLookupSubset <- markerTable %>% 
    filter(Trend == inputTrend)
  
  markerDescription <- markerLookupSubset$MarkerDescription
  
  return(markerDescription)
}

# END FUNCTION - markerLookup

# Load the  site  listing from the InputFiles folder
InstSites <- read.csv(paste0(MainDir,"InputFiles/2018InstSites.csv"), 
                  header = TRUE, colClasses = c("character", "character","character"))

# Load National aquifer code reference list

NatAqfrCds <- read.csv(paste0(MainDir,"InputFiles/NationalAquiferCodeListing.csv"), 
                     header = TRUE, colClasses = c("character", "character","character", "character", "character","character", "character"))
localAqfrCds <- read.csv(paste0(MainDir,"InputFiles/LocalAquiferCodeListing.csv"), 
                       header = TRUE, colClasses = c("character", "character","character"))
# Get the site file information for each site
# CALL USGS FUNCTION readNWISsite
SiteInfo <- readNWISsite(InstSites$SiteID) 
GwSI_File_Rec <- SiteInfo %>% 
  select(dec_long_va, dec_lat_va, site_no, station_nm, well_depth_va, hole_depth_va,
         aqfr_cd, alt_va, county_cd, land_net_ds, dec_coord_datum_cd)

# Save the results as an excel file
write.xlsx(GwSI_File_Rec,paste(MainDir,"tables/InstGWSI_File.xlsx", sep=''))
write.csv(GwSI_File_Rec,paste(MainDir,"tables/InstGWSI_File.csv", sep=''))


# Create the list of sites
Usites<-InstSites$SiteID


#
# END INTIAL SET UP
#--------------------------------------------------------------------------------------------------
#
#__________________________________________________________________________________________________
#**************************************************************************************************
#**************************                     ***************************************************
# *************************    BEGIN MAIN LOOP  ***************************************************
# *************************                     ***************************************************
#**************************************************************************************************
#
# Begin a for loop that goes through the file and makes the graphs and tables for each site.

 for(current_site in Usites){ # for loop ends at line 942 - comment out for testing
# current_site<-"260041080093102"
  
  

# assign the site number and site name from the current site of the loop and the site info 
  siteNumber<-current_site
  RewordText<-"No"
  print(current_site)
# _________________________________________________________________________________________________
# GENERATE SITE INFORMATION TEXT    
# Based on the current site parse out the information to create the station summary which includes
# Site ID, Site Name, Aquifer code, Aquifer Name, LSD, and site location info
  current_site_info <- SiteInfo %>% filter(site_no == current_site)
  current_site_aquifer<-NatAqfrCds %>% filter(Code == current_site_info$nat_aqfr_cd)
  current_site_localaqfr<-localAqfrCds %>% filter(aqfr_cd == current_site_info$aqfr_cd)
  AvailableData<-whatNWISdata(siteNumber = current_site)
  siteNumber<-current_site
  siteName<-current_site_info$station_nm
  print(siteName)
  outputfilname<-paste0(siteNumber,siteName,".txt")
  
  lt <- current_site_info$lat_va
  ln <- current_site_info$long_va
  hdatum<-current_site_info$coord_datum_cd
  latlongtxt <- sprintf("latitude %s°%s'%s\", longitude %s°%s'%s\" ",
                 substr(lt,1,2),substr(lt,3,4),substr(lt,5,6),
                 substr(ln,1,2),substr(ln,3,4),substr(ln,5,6))
locationText1<-paste0("Location: ",latlongtxt, hdatum, ", landnet ", current_site_info$land_net_ds)
locationText2<-paste0("          ", countyCdLookup(current_site_info$state_cd, current_site_info$county_cd,  output = "fullName"),", ",stateCdLookup(current_site_info$state_cd, "fullName"),", hydrologic unit ", current_site_info$huc_cd)
depthText<-paste0("Well depth: ", current_site_info$well_depth_va, " feet")
altitudeText<-paste0("Land surface altitude: ", current_site_info$alt_va, " feet above ", current_site_info$alt_datum_cd)
NatAqfrCdText<-paste0("Aquifer:  National designation - ", current_site_aquifer$Aquifer.Name, " (", current_site_info$nat_aqfr_cd, "), local designation - ", current_site_localaqfr$Aqfr_Name_prpr, " (", current_site_info$aqfr_cd,")")
cat(locationText1)
  sink(paste0(MainDir,"HeaderFiles/",outputfilname))
  cat(locationText1)
  cat("\n")
  cat(locationText2)
  cat("\n")
  cat(NatAqfrCdText)
  cat("\n")
  cat(depthText)
  cat("\n")
  cat(altitudeText)
  cat("\n")
  
  sink()
  # file.show(paste0(MainDir,"HeaderFiles/",outputfilname))
  
# END GENERATING SITE INFORMATION TEST
# ----------------------------------------------------------------------------------------------------------

#__________________________________________________________________________________________________________
#
# PULL  INSTANTANEOUS DATA FOR SITE AND SITE, VARIABLE, AND STATISTIC INFO and SET PATHS and LABLES
#

# Read the instantaneous water level data

instGWLevelData <- readNWISgwl(siteNumber)



# Get the info on the parameter code this is where the variable description used for the plot comes from.


# Create the title for all plots, and plot files, and the path for plot files
FullSiteName<-paste(siteNumber,siteName)
path <- paste(MainDir, "plots/", FullSiteName, "_", "FrqA", ".png", sep='')
# 
# END PULLING  IV DATA
#---------------------------------------------------------------------------------------

# _____________________________________________________________________________________
# SET UP INFORMATION FOR PLOTTING
#
# Create columns in gwlevel with month, day of year, year, week of year, the abbreviated month

instGWLevelData$month <- as.POSIXlt(instGWLevelData$lev_dt)$mon + 1
instGWLevelData$doy <- as.POSIXlt(instGWLevelData$lev_dt)$yday + 1
instGWLevelData$year <- as.POSIXlt(instGWLevelData$lev_dt)$year + 1900
instGWLevelData$NewDate <- as.Date(instGWLevelData$lev_dt, format="%d-%m-%Y")
instGWLevelData$woy <- as.numeric( format(instGWLevelData$NewDate+0, "%U"))
instGWLevelData$AbbMonth <- month.abb[instGWLevelData$month]

# Convert Abbreviated months to USGS abbreviated months
instGWLevelData$AbbMonth[instGWLevelData$AbbMonth=="Jan"]<-"Jan."
instGWLevelData$AbbMonth[instGWLevelData$AbbMonth=="Feb"]<-"Feb."
instGWLevelData$AbbMonth[instGWLevelData$AbbMonth=="Mar"]<-"Mar."
instGWLevelData$AbbMonth[instGWLevelData$AbbMonth=="Apr"]<-"Apr."
instGWLevelData$AbbMonth[instGWLevelData$AbbMonth=="May"]<-"May"
instGWLevelData$AbbMonth[instGWLevelData$AbbMonth=="Jun"]<-"June"
instGWLevelData$AbbMonth[instGWLevelData$AbbMonth=="Jul"]<-"July"
instGWLevelData$AbbMonth[instGWLevelData$AbbMonth=="Aug"]<-"Aug."
instGWLevelData$AbbMonth[instGWLevelData$AbbMonth=="Sep"]<-"Sept."
instGWLevelData$AbbMonth[instGWLevelData$AbbMonth=="Oct"]<-"Oct."
instGWLevelData$AbbMonth[instGWLevelData$AbbMonth=="Nov"]<-"Nov."
instGWLevelData$AbbMonth[instGWLevelData$AbbMonth=="Dec"]<-"Dec."


DataPoints<-length(instGWLevelData$sl_lev_va)
YearsWithRecord<-length(unique(instGWLevelData$year))
MonthsWithRecord<-length(unique(instGWLevelData$month))
EnoughData<-YearsWithRecord > 5



# determine the current date, day, and week
Cdate<-Sys.Date()
# Cdate<-as.Date("2018-01-27",origin = "1960-10-01") # : This is for testing code for when to start 2 year and 1 year graphs. Different dates can be tried.
currentDay<-as.numeric( format(Cdate, "%d"))
currentWeek <-strftime(as.POSIXlt(Cdate),format="%W")
currentYear <-strftime(as.POSIXlt(Cdate),format="%Y")
currentMonth <-strftime(as.POSIXlt(Cdate),format="%m")
currentMonth <-as.numeric(currentMonth)

# Determine the date, month, and week to begin and end the 1 year and 2 year graphs
if(currentMonth <12){startMonth<- as.POSIXlt(Cdate)$mon + 2} else {startMonth<- as.POSIXlt(Cdate)$mon -10}
if(currentMonth <12){startYear<-as.POSIXlt(Cdate)$year + 1900-1} else {startYear<-as.POSIXlt(Cdate)$year + 1900}
startDay<-1
startDate<-as.Date(paste0(startYear,"-",startMonth,"-", startDay))
startDate2yr<-as.Date(paste0(startYear-1,"-",startMonth,"-", startDay))
EndDate<-as.Date(paste0(startYear+1,"-",startMonth,"-", startDay))

startWeek<-strftime(startDate, format="%W")
startWeek<-as.numeric(startWeek)

# Pull the last two records and see how much time had passed between them and the change in water levels during that period

lastReading<-instGWLevelData[length(instGWLevelData[,1]),]
DateLastReading<-lastReading$NewDate
DaysSinceLastReading<-Cdate-DateLastReading
ValueLastReading<-lastReading$sl_lev_va
PreviousReading<-instGWLevelData[length(instGWLevelData[,1])-1,]
ChangeSincePreviousReading<-lastReading$sl_lev_va - PreviousReading$sl_lev_va
DaysSincePreviousReading<-lastReading$lev_dt - PreviousReading$lev_dt

# END SET UP

###############################################################################
#                                                                             #
###################  Seasonal Kendall Trend Test ##############################
#                                                                             #
###############################################################################

# Criteria for trend testing for instantaneous sites - To be comparable to the trend testing for continuous  recorders 
# there has to be at least monthly data for 5 years, and if there is monthly data for at least 20 years that test will also be conducted.
# 
# Determine the dates of the last 5 and twenty years of data and pull that data from the table instGWLevelData

# Set up an empty table

FiveYearResults<-c(rep(NA, 5))
TwentyYearResults<-c(rep(NA, 5))
TrendTestResults<-as.data.frame(rbind(FiveYearResults,TwentyYearResults))
names(TrendTestResults)<-c("tau", "pValue", "slope", "intercept", "trend")
# If there is enough data run the test
if(EnoughData){
  DateFiveYears <- as.Date(DateLastReading)
  year(DateFiveYears)<-year(DateFiveYears)-5
  DataLastFiveYears<-instGWLevelData %>%
    filter(NewDate > DateFiveYears)
  DateTwentyYears <- as.Date(DateLastReading)
  year(DateTwentyYears)<-year(DateTwentyYears)-20
  DataLastTwentyYears<-instGWLevelData %>%
  filter(NewDate > DateTwentyYears)
  # Refine the test for enough data, for 5 year trend test it must at least 29 records which is equivalent to monthly data that is 80% complete
  # for twenty year test, it must at least 192 records which is equivalent to monthly data that is 50% complete
  # is there a trend? 
  # is it statistically significant? 
  # is it positive or negative?
  if(length(DataLastFiveYears[,1]) > 29){
    FiveYearTrendInfo <- kendallSeasonalTrendTest(sl_lev_va ~ month + year, data = DataLastFiveYears)
    TrendTestResults$tau[1] = FiveYearTrendInfo$estimate['tau']
    TrendTestResults$pValue[1] = FiveYearTrendInfo$p.value[2]
    TrendTestResults$slope[1] = FiveYearTrendInfo$estimate['slope']
    TrendTestResults$intercept[1] = FiveYearTrendInfo$estimate['intercept']
    if(TrendTestResults$tau[1] == 0){ 
      TrendTestResults$trend[1] <- "None"
    } else if(TrendTestResults$pValue[1] >= 0.05){ 
      TrendTestResults$trend[1] <- "Not significant"
    } else { 
      TrendTestResults$trend[1] <- ifelse(TrendTestResults$slope[1] > 0, "Up", "Down")
    }
  }else{TrendTestResults$trend[1]<-"Insufficient data"}
  # 20 year trend test if there is at least 20 years of data
  # is there a trend? 
  # is it statistically significant? 
  # is it positive or negative?
  if(length(DataLastTwentyYears[,1]) > 120){
    TwentyYearTrendInfo <- kendallSeasonalTrendTest(sl_lev_va ~ month + year, data = DataLastTwentyYears)
    TrendTestResults$tau[2] = TwentyYearTrendInfo$estimate['tau']
    TrendTestResults$pValue[2] = TwentyYearTrendInfo$p.value[2]
    TrendTestResults$slope[2] = TwentyYearTrendInfo$estimate['slope']
    TrendTestResults$intercept[2] = TwentyYearTrendInfo$estimate['intercept']
    if(TrendTestResults$tau[2] == 0){ 
      TrendTestResults$trend[2] <- "None"
    } else if(TrendTestResults$pValue[2] >= 0.05){ 
      TrendTestResults$trend[2] <- "Not significant"
    } else { 
      TrendTestResults$trend[2] <- ifelse(TrendTestResults$slope[2] > 0, "Up", "Down")
    }
  }else{TrendTestResults$trend[2]<-"Insufficient data"}
}else{
  TrendTestResults$trend[1]<-"Insufficient data"
  TrendTestResults$trend[2]<-"Insufficient data"}
  
# Determine marker for the 5- and 20-year trends

FiveYearSymbol<-markerLookup(TrendTestResults$trend[1])
TwentyYearSymbol<-markerLookup(TrendTestResults$trend[2])



#___________________ Kendall Trend Test - Run using same monthly data as the Kendall Seasonal Trend Test ___________________________
# 
# This code takes the data used for the Kendall Seasonal Trend Test. Takes the existing month and year info used for the 20 year Kendall
# seasonal trend test, and creates a date column that could be used for comparision purposes, adn then runs just a regular kendall trend test.
# it prints out the results of both trend tests for comparison

if(EnoughData){
  KTT_20Yr_TrendResults<-kendallTrendTest(y = DataLastTwentyYears$sl_lev_va ~ DataLastTwentyYears$NewDate)
  KTT_5Yr_TrendResults<-kendallTrendTest(y = DataLastFiveYears$sl_lev_va ~ DataLastFiveYears$NewDate)
} 


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
  
 


# -------------------------------------------------------------------------------------------------
#__________________________________________________________________________________________________
# DETERMINE CHANGE SINCE PREVIOUS READING and SET UP ARROWS FOR MAP
#

if(length(ChangeSincePreviousReading)==0){
  
  WLsymbol <- "Xsymbol"
  WLChangeDir <- "missing data"
  WLChange <- NA
  #print("I am in length test")
}else{
  if(is.na(ChangeSincePreviousReading) ){
    WLsymbol <- "Xsymbol"
    WLChangeDir <- "missing data"
    WLChange <- NA} else{
      
      WLChange <- round(ChangeSincePreviousReading, digits = 2)
      
      if(ChangeSincePreviousReading < 0 ){
        WLChangeDir <- "down"
        WLsymbol <- "ThinDownArrow"
      }
      if(ChangeSincePreviousReading == 0 ){
        WLChangeDir <- "no change"
        WLsymbol <- "Circle"
      }
      if(ChangeSincePreviousReading > 0 ){
        WLChangeDir <- "up"
        WLsymbol <- "ThinUpArrow"
      }
      
    } 
  
}

# END SETTING UP ARROWS




# The information above will be summerized in the file SummaryWLChanges and combined with the frequency analysis results.
# -------------------------------------------------------------------------------------------------
#
# _________________________________________________________________________________________________
#
# ************ BEGIN PERCENTILE RANKING **********************************************************
#

# Get the data for the current week of the year
if(EnoughData){
  Data4MonthOfLastReading<-instGWLevelData%>% 
    filter(month==lastReading$month)
  # Create a function to determine the percetile ranks of the water levels for the week of the year of the last data point,
  # and use this to determine the percentile rank of the last water level
  perc.rank <- function(x) trunc(rank(x, na.last = "keep"))/length(x)
  Data4MonthOfLastReading <- within(Data4MonthOfLastReading, xr <- perc.rank(Data4MonthOfLastReading$sl_lev_va))
  Data4LastReading<-Data4MonthOfLastReading  %>%
    filter(NewDate == DateLastReading)
  PercRankofLastReading<-Data4LastReading$xr[1]
  PercentileRanKLastRead <- round(PercRankofLastReading*100, digits = 1)
} else{
  PercentileRanKLastRead<-NA
}
#************** End Percentile Ranking ************************************************************
#**************** SET UP FOR PERCENTILE ANALYSIS PLOTS *******************************************
# Get the last year of data
lastYearofData<-instGWLevelData%>%
  filter(NewDate >= startDate)
  if(nrow(lastYearofData)>0){
    if(EnoughData){
      
      
      # Create a column in each dataset that can be used to put it back in the original original order.
      lastYearofData$OrgOrder<-c(1:nrow(lastYearofData))
      # this with the last year of data. This puts NAs where there is no data. 
      fullYear<-as.data.frame(seq(as.Date(startDate), as.Date(EndDate), by="days"))
      colnames(fullYear)<-"NewDate"
      lastYearofData<-merge(fullYear, lastYearofData, by="NewDate", all.x=TRUE)
      
      
      # FREQUENCY ANALYSIS - Compute the min and max median
      annualStats <- instGWLevelData %>%
        group_by(year,month) %>%
        summarize(median = median(sl_lev_va)) %>%
        group_by(month) %>%
        summarise(minMed = min(median, na.rm=TRUE),
                  maxMed = max(median, na.rm=TRUE))
      # FREQUENCY ANALSIS - Compute the quantiles
      gwlevelStats <- instGWLevelData %>%
        group_by(month) %>%
        summarise(p10 = quantile(sl_lev_va, probs=0.1, na.rm=TRUE),
                  p25 = quantile(sl_lev_va, probs=0.25, na.rm=TRUE),
                  p50 = quantile(sl_lev_va, probs=0.5, na.rm=TRUE),
                  p75 = quantile(sl_lev_va, probs=0.75, na.rm=TRUE),
                  p90 = quantile(sl_lev_va, probs=0.9, na.rm=TRUE),
                  nYears = length(unique(year)),
                  p5 = quantile(sl_lev_va, probs=0.05, na.rm=TRUE),
                  p95 = quantile(sl_lev_va, probs=0.95,na.rm=TRUE ))
      # Set up the limits for the quantile plots. Min and Max of measurments are not used because for the test
      # well the max was much larger than the quantiles.
      ymax<-max(instGWLevelData$sl_lev_va, na.rm=TRUE)
      ymin<-min(instGWLevelData$sl_lev_va, na.rm=TRUE)
      Rangefreq<-ymax-ymin
      plotrange<-abs(ymax-ymin)
      ymax<-ymax+plotrange*0.1
      ymin<-ymin-plotrange*0.1
      yLimits <- c(ymin, ymax)
      MainStats <- merge(annualStats, gwlevelStats, 
                         by="month") 
      gwlevelStats <- merge(annualStats, gwlevelStats, 
                            by="month")
      MainStats2<-merge(lastYearofData, MainStats, by="month")
      attach(MainStats2)
      MainStats2<-MainStats2[order(OrgOrder),]
      detach(MainStats2)
      finalStats <- gwlevelStats%>%
        select(month, minMed, p10, p25, p50, p75, p90,  maxMed, nYears)
      allMonths<-as.data.frame(c(1:12))
      colnames(allMonths)<-"month"
      finalStats<-merge(allMonths, finalStats, by="month", all.x=TRUE)
      # Create output table with percentiles
      monthNames<-c("January","February","March","April","May","June","July","August","September","October","November","December")
      finalstatsOut<-cbind(monthNames, round(finalStats, digits = 2))
      names(finalstatsOut)<-c("Month of the year", "Month Number", "Lowest median", "10th percentile", "25th percentile", "50th percentile", "75 percentile", "90th percentile", "Highest median", "Number of years")
      StatsForLastReading <- finalstatsOut %>% filter(`Month Number` == Data4LastReading$month)
      finalstatsOut<-within(finalstatsOut, rm("Month Number") )
      PercentileTabPath <- paste0(MainDir,"tables/",FullSiteName,"percentiles",".html")
      print(xtable(finalstatsOut), type="html", file=PercentileTabPath, include.rownames=FALSE)
      # End of creating output table with percentiles
      # Create an output table with the date of last reading, the value, the color code for the symbol, and the change in the last week. 
      
      if(is.na(Data4LastReading$sl_lev_va)){
        WLfreqResult<-"Missing last reading"
        RewordText<-"Yes"
        SymbolCol<-"Gray"
        ColRGB<-"R191G191B191"
      }else{
        if(Data4LastReading$sl_lev_va < StatsForLastReading$`10th percentile`){
          WLfreqResult<-"below the 10th percentile"
          SymbolCol<-"TuscanRed"
          ColRGB<-"R168G0B0"}
        if(Data4LastReading$sl_lev_va >= StatsForLastReading$`10th percentile` & Data4LastReading$sl_lev_va < StatsForLastReading$`25th percentile` ){
          WLfreqResult<-"within the 10th to 24th percentiles"
          SymbolCol<-"MarsRed"
          ColRGB<-"R255G0B0"}
        if(Data4LastReading$sl_lev_va >= StatsForLastReading$`25th percentile` & Data4LastReading$sl_lev_va < StatsForLastReading$`50th percentile` ){
          WLfreqResult<-"within the 25th to 49th percentiles"
          SymbolCol<-"MediumApple"
          ColRGB<-"R85G255B0"}
        if(Data4LastReading$sl_lev_va >= StatsForLastReading$`50th percentile` & Data4LastReading$sl_lev_va <= StatsForLastReading$`75 percentile` ){
          WLfreqResult<-"within the 50th to 75th percentiles"
          SymbolCol<-"MediumApple"
          ColRGB<-"R85G255B0"}
        if(Data4LastReading$sl_lev_va > StatsForLastReading$`75 percentile`  & Data4LastReading$sl_lev_va <= StatsForLastReading$`90th percentile` ){
          WLfreqResult<-"within the 76th to 90th percentiles"
          SymbolCol<-"SodaliteBlue"
          ColRGB<-"R190G232B255"}
        if(Data4LastReading$sl_lev_va > StatsForLastReading$`90th percentile`){
          WLfreqResult<-"above the 90th percentile"
          SymbolCol<-"ApatiteBlue"
          ColRGB<-"R115G223B255"} 
      }
      
    }else{
      WLfreqResult<-"Insufficient data"
      RewordText <-"Yes"
      SymbolCol<-"Gray"
      ColRGB<-"R191G191B191"
    }
    
    
  }else{
    WLfreqResult<-"Insufficient data"
    RewordText <-"Yes"
    SymbolCol<-"Gray"
    ColRGB<-"R191G191B191"
  }
    




    

# *************  End PERCENTILE RANKING ******************************************************************************
# --------------------------------------------------------------------------------------------------------------------
#
# ___________________________________________________________________________________________________________________
#
# CREATE TABLE WITH SUMMARY INFO FOR EACH SITE 

SummaryWLChanges <- data.frame(current_site, siteName, current_site_info$dec_lat_va, current_site_info$dec_long_va, as.character(DateLastReading), ValueLastReading, WLChange,  paste0(SymbolCol,"-",WLsymbol), ColRGB, WLfreqResult, PercentileRanKLastRead,  current_site_info$aqfr_cd, DaysSinceLastReading, TrendTestResults$trend[1], paste0(SymbolCol,"-",FiveYearSymbol),TrendTestResults$trend[2],paste0(SymbolCol,"-",TwentyYearSymbol), paste0(SymbolCol,"-",finalLTSymbol),  DataPoints, YearsWithRecord, MonthsWithRecord, current_site_info$alt_datum_cd)

if(exists("TableWLChanges")) {TableWLChanges<-rbind.data.frame(TableWLChanges, SummaryWLChanges)} else {TableWLChanges<-SummaryWLChanges}

# ____________________________________________________________________________________________________
# ****************************************************************************************************
# ************************** MAKE DATE OF GRAPH TEXT  ********************************************
# ****************************************************************************************************
DateGraphText<-paste0("Graph created ", Cdate," - Includes provisional data")

#***************************** TEST YEAR OF DATA **********************************************************************

if(nrow(lastYearofData)>0){
  
  #************************** PERCENTILE ANALYSIS *********************************************************************
  
  if(EnoughData){
    # ___________________________________________________________________________________________________________________
    #
    # ***************************  REARRAGE QUANTILES FOR MAKING INTO BOX PLOTS ******************************************
    #
    
    # Grab only the quantiles from finalStats
    
    quantData <- finalStats[,c(-1,-9)]
    
    plotData <- quantData
    
    # This block of code converts the quantiles into bar segments that stack on top of eachother to form the 
    # frequency analysis on a month by month basis. Together all the segments add up to the maxMed
    for(i in 2:ncol(plotData)){
      plotData[,i] <- quantData[,i] - quantData[,i-1]
    }
    
    # I think this block of code determins the width of each bar, and adds it as a new column.
    # below I revise this code to the number of days in a week.
    
    #plotData[,i+1] <- rep(10*max(instGWLevelData$sl_lev_va),nrow(plotData))
    
    plotData[,i+1] <-c(31,28.25,31, 30, 31, 30, 31, 31, 30, 31, 30, 31)/30.25
    
    stats<-t(as.matrix(plotData))
    
    # rearrange the statistics to be in order of data for the last year
    if(currentMonth == 12){
      monthsToplot <- 1:ncol(stats)
    } else {
      
      monthsToplot <- c((currentMonth+1):ncol(stats),1:(currentMonth))
    }
    stats <- stats[,monthsToplot]
    quantData <- quantData[monthsToplot,]
    
    # Create a dataframe that has the heights for months, the month abbreviation, and the month widths
    MonthInfo<-rbind(
      rep(yLimits[2], 12),
      c("Jan.","Feb.","Mar.", "Apr.", "May","June","July","Aug.","Sept.","Oct.", "Nov.", "Dec."),
      c(31,28.25,31, 30, 31, 30, 31, 31, 30, 31, 30, 31)/30.25
      # rep(1,12)
    )
    
    # rearrange the month info to be in same order as the data and stats for the last year
    if(startMonth != 1){
      monthsToplot2 <- c((startMonth):ncol(MonthInfo),1:startMonth-1)
    } else {
      monthsToplot2 <- 1:ncol(MonthInfo)
    }
    
    MonthInfo<-MonthInfo[,monthsToplot2]
    
    # a different MonthInfo to try to overlay months with limits of 
    MonthInfo2<-rbind(
      rep(yLimits[2], 12),
      rep(yLimits[1], 12)
    )
    MonthInfo3<-rbind(
      MonthInfo2[1,],
      MonthInfo2[2,]-MonthInfo2[1,]
      
    )
    
    MonthInfo3<-MonthInfo3[,monthsToplot2]
    
    
    
    
    # ____________________________________________________________________________________________________
    # ****************************************************************************************************
    # ************************** MAKE FREQUENCY ANALYSIS PLOT ********************************************
    # ****************************************************************************************************
    if(nrow(lastYearofData)>0){
      xlimDays<-c(0,nrow(lastYearofData))
      XTitle<-paste(format(as.Date(as.character(startDate)),format= "%B %Y"), 
                    " to ", format(as.Date(as.character(EndDate)),format= "%B %Y"))
      
      png(file=path, width = 700, height = 600)
      par(oma = c(4, 1, 1, 1))
      par(mar = c(6, 4, 3, 2)+0.1)
      
      bp<- barplot(stats, 
                   xlab=XTitle, 
                   ylab="Water level in feet", 
                   main=paste(c(siteNumber, siteName)),
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
      points(bp, quantData$p50, pch=17, cex=1.5, col="green")
      
      box()
      bp<-barplot(MonthInfo3, width=as.numeric(MonthInfo[3,]), names.arg=MonthInfo[2,], xaxs = "i", ylim=yLimits,  xpd=FALSE,  col=NA, border = "gray70", space=0, add=TRUE)
      
      box()
      
      # Plot the instantaneous values
      
      
      par(new=TRUE)
      ptsx_A<-c(1:length(lastYearofData$NewDate))
      ptsy_A<-lastYearofData$sl_lev_va
      plot(lastYearofData$sl_lev_va,
           ylim=yLimits,  axes=FALSE,xaxs = "i", xlab="",ylab="", lty=1, pch=19, cex=2, col="black")
      lines(ptsx_A, ptsy_A, xlim=xlimDays, ylim=yLimits, pch=16, lwd=2, col="black")
      
      
      
      # Add notes to plots
      mtext("U.S. Geological Survey", adj=0, col="dark green")
      mtext(DateGraphText, side=3, adj=1,col="black", cex=0.8)
      
      par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE, xpd=TRUE) # this sets the legend up
      legend("bottomleft", 
             c("EXPLANATION", "   Measured water levels", "   Median of historic water levels", "   Percentile classes (shown separately)", "   are compiled by week of year"), 
             
             pch=c(NA, 16, 17, NA, NA),
             col=c("black", "black",  "green", "black", "black"),
             merge=FALSE,
             xpd = TRUE, 
             horiz = FALSE, 
             inset = c(0, -0.22)
      )
      
      
      
      par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE, xpd=TRUE) # this sets the legend up
      legend("bottomright", 
             c("Percentile classes", "   > 90", "   76-90","   25-75", "   10-24", "   < 10"), 
             # lty=c(NA,NA, NA, NA, NA, NA), 
             pch=c(NA,15,15,15, 15, 15),
             col=c(NA, "lightblue2", "lightcyan", "darkolivegreen1", "lightpink", "palevioletred1"),
             merge=FALSE,
             xpd = TRUE, 
             horiz = FALSE, 
             inset = c(0, -0.22)
      )
      
      dev.off()
      # ----------------------- END FREQUECY ANALYSIS PLOT -------------------------------------------------
      
    }
    
  }
  
}




# ____________________________________________________________________________________________________
# ****************************************************************************************************
# *********************** MAKE INSTANTANEOUS DATA PLOT  **********************************************
# ****************************************************************************************************


INSTpath <- paste(MainDir, "plots/", FullSiteName, "_", "INST", ".png", sep='')
print(INSTpath)
png(file=INSTpath, width = 700, height = 600)
par(oma = c(4, 1, 1, 1))
par(mar = c(6, 4, 3, 2)+0.1)

plot(instGWLevelData$lev_dt,instGWLevelData$sl_lev_va, type = "p", col="blue",  main=paste(c(siteNumber, siteName)),
     xlab = "Date", ylab = "Water level in feet", tck=0.05)
axis(3, labels=FALSE, tck=0.025)
axis(4, labels=FALSE, tck=0.025)
mtext("U.S. Geological Survey", adj=0, col="dark green")
mtext(DateGraphText, side=3, adj=1,col="black", cex=0.8)
par(xpd=FALSE)

# Add trend lines to the graph if the trends are statistically significant
if(TrendTestResults$trend[1] == 'Up' || TrendTestResults$trend[1] == 'Down'){
  FiveyrLineStartDate<-as.numeric(as.Date(as.character(DateLastReading)))-5*365.25
  FiveyrLineEndDate<-as.numeric(as.Date(as.character(DateLastReading)))
  FiveyrFirstValue<-FiveyrLineStartDate *(KTT_5Yr_TrendResults$estimate['slope'])+ KTT_5Yr_TrendResults$estimate['intercept']
  FiveyrEndValue<-FiveyrLineEndDate *(KTT_5Yr_TrendResults$estimate['slope'])+ KTT_5Yr_TrendResults$estimate['intercept']
  segments(x0 = FiveyrLineStartDate, x1= FiveyrLineEndDate, y0 = FiveyrFirstValue, y1 = FiveyrEndValue,  col="forestgreen", lty=5, lwd = 2)
}

if(TrendTestResults$trend[2] == 'Up' || TrendTestResults$trend[2] == 'Down'){
  TwentyyrLineStartDate<-as.numeric(as.Date(as.character(DateLastReading)))-20*365.25
  TwentyyrLineEndDate<-as.numeric(as.Date(as.character(DateLastReading)))
  TwentyyrFirstValue<-TwentyyrLineStartDate *(KTT_20Yr_TrendResults$estimate['slope'])+ KTT_20Yr_TrendResults$estimate['intercept']
  TwentyyrEndValue<-TwentyyrLineEndDate *(KTT_20Yr_TrendResults$estimate['slope'])+ KTT_20Yr_TrendResults$estimate['intercept']
  segments(x0 = TwentyyrLineStartDate, x1= TwentyyrLineEndDate, y0 = TwentyyrFirstValue, y1 = TwentyyrEndValue,  col="forestgreen", lty=5, lwd = 2)
}    


par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE, xpd=TRUE) # this sets the legend up
legend("bottomright", 
       c("EXPLANATION",  "    Manual water level measurements", "5- and 20-year trend lines (shown if statiscally significant)"), 
       lty=c(NA,NA,5),
       pch=c(NA,1),
       col=c("black",  "blue","forestgreen"),
       merge=FALSE,
       xpd = TRUE, 
       horiz = FALSE, 
       inset = c(0, -0.22)
)



dev.off()



# _________________________________________________________________________________________________
# *************************************************************************************  
# **************************                                  ************************* 
# **************************  GENERATE TEXT FOR STATION PAGES *************************
# **************************                                  *************************
# *************************************************************************************

# GENERATE SITE INFORMATION TEXT

# Based on the current site parse out the information to create the station summary which includes
# Site ID, Site Name, Aquifer code, Aquifer Name, LSD, and site location info

outputfilname<-paste0(siteNumber,siteName,".txt")

lt <- current_site_info$lat_va
ln <- current_site_info$long_va
hdatum<-current_site_info$coord_datum_cd
latlongtxt <- sprintf("latitude %s°%s'%s\", longitude %s°%s'%s\" ",
                      substr(lt,1,2),substr(lt,3,4),substr(lt,5,6),
                      substr(ln,1,2),substr(ln,3,4),substr(ln,5,6))
locationText1<-paste0("Location: ",latlongtxt, hdatum, ", landnet ", current_site_info$land_net_ds)
locationText2<-paste0("          ", countyCdLookup(current_site_info$state_cd, current_site_info$county_cd,  output = "fullName"),", ",stateCdLookup(current_site_info$state_cd, "fullName"),", hydrologic unit ", current_site_info$huc_cd)
depthText<-paste0("Well depth: ", current_site_info$well_depth_va, " feet")
altitudeText<-paste0("Land surface altitude: ", current_site_info$alt_va, " feet above ", current_site_info$alt_datum_cd)
NatAqfrCdText<-paste0("Aquifer:  National designation - ", current_site_aquifer$Aquifer.Name, " (", current_site_info$nat_aqfr_cd, "), local designation - ", current_site_localaqfr$Aqfr_Name_prpr, " (", current_site_info$aqfr_cd,")")
cat(locationText1)
sink(paste0(MainDir,"HeaderFiles/",outputfilname))
cat(locationText1)
cat("\n")
cat(locationText2)
cat("\n")
cat(NatAqfrCdText)
cat("\n")
cat(depthText)
cat("\n")
cat(altitudeText)
cat("\n")
sink()
#file.show(paste0(MainDir,"HeaderFiles/",outputfilname))

# END GENERATING SITE INFORMATION TEST

# GENERATE TEXT NEXT TO FREQUENCY ANALYSIS PLOT
FreqTextname<-paste0(siteNumber,siteName,"frq.txt")
# Generate the text
Sentence1 <-paste0("Manually measured water level above ",current_site_info$alt_datum_cd, ", in feet.")
Sentence2 <-paste0("The most recent water level, measured on ", DateLastReading, ", is ", ValueLastReading, " feet.")

if(RewordText =="Yes"){Sentence3 <-"There is insufficient data for comparison with a frequency analysis of historical water levels."} else {Sentence3 <-paste0("This value is ", WLfreqResult," for the month of ", StatsForLastReading$`Month of the year`,".")}

cat(Sentence1)
sink(paste0(MainDir,"HeaderFiles/", FreqTextname))
cat(Sentence1)
cat("\n")
cat(Sentence2)
cat("\n")
cat(Sentence3)
cat("\n")
sink()
#file.show(paste0(MainDir,"HeaderFiles/",FreqTextname))


# GENERATE TEXT FOR PERIODIC DATA SUMMARY


periodicDataSummary<-as.matrix(summary(as.vector(instGWLevelData$sl_lev_va)))
if(length(row.names(periodicDataSummary))==6){row.names(periodicDataSummary)<-c("Minimum", "1st quartile", "Median", "Mean", "3rd quartile", "Maximum")}
if(length(row.names(periodicDataSummary))==7){row.names(periodicDataSummary)<-c("Minimum", "1st quartile", "Median", "Mean", "3rd quartile", "Maximum", "Number of missing values")}
periodicDataSummary<-as.data.frame(t(periodicDataSummary))
periodicTextname<-paste0(siteNumber,siteName,"periodic.txt")

# CREATE A TABLE FOR THE DATA SUMMARY

print(xtable(periodicDataSummary), type="html", file=paste0(MainDir,"tables/", FullSiteName,"periodicDataSummary", ".html"), include.rownames=FALSE)

# Generate the text
Sentence1c <-paste0("Summary for manual water level measurements above ", current_site_info$alt_datum_cd, ", in feet.")
Sentence2c <- "Manually measured water levels are available for the period"
Sentence3d <- paste0(min(instGWLevelData$lev_dt)," to ", max(instGWLevelData$lev_dt),".")
Sentence4c <- paste0("During this period, water level ranged from")
Sentence5c <- paste0(round(periodicDataSummary$Minimum, digits = 2)," to ", round(periodicDataSummary$Maximum, digits = 2), " feet, and averaged ", round(periodicDataSummary$Mean, digits = 2), " feet.")
cat(Sentence1c)
sink(paste0(MainDir,"HeaderFiles/", periodicTextname))
cat(Sentence1c)
cat("\n")
cat(Sentence2c)
cat("\n")
cat(Sentence3d)
cat("\n")
cat(Sentence4c)
cat("\n")
cat(Sentence5c)
cat("\n")
sink()
#file.show(paste0(MainDir,"HeaderFiles/",periodicTextname))


# ____________________________________________________________________________________________________
# ****************************************************************************************************
# ******************************* CREATE OUTPUT TABLES  **********************************************
# ****************************************************************************************************



TablepathLTT <- paste0(MainDir,"tables/", FullSiteName, "_LTTrend", ".html")
row.names(TrendTestResults)<-c("Five-year trend test results", "Twenty-year trend test results")
print(xtable(TrendTestResults), type="html", file=TablepathLTT, include.rownames=TRUE)


# Instantaeous Water Level Table 
TablepathIV <- paste0(MainDir,"tables/", FullSiteName, "_IV", ".html")
IVtable <- instGWLevelData[,c(2,4,8,9)]
names(IVtable)<- c("USGS site identifier", "Date", "Water level in feet", "Datum")
IVtable$Date<-as.character(IVtable$Date)
print(xtable(IVtable), type="html", file=TablepathIV, include.rownames=FALSE)

}
 
data.frame(TableWLChanges, row.names = NULL)

names(TableWLChanges) <-c("site_no","site_nm", "dec_lat_va","dec_long_va", "last_wl_dt", "wl",  "wl_change", "symbol",  "color_rgb","freq", "perc",  "Aquifer", "days_of_last_value", "trend_5year", "FiveYearSymbol", "trend_20year", "TwentyYearSymbol", "long_term_symbol",  "data_points", "years", "months", "Vertical_Datum")

 SumTable_112BSCNN <-TableWLChanges %>% filter(Aquifer == "112BSCNN")
 SumTable_110SAQS <-TableWLChanges %>% filter(Aquifer == "110SAQS")
 SumTable_122SNDS <-TableWLChanges %>% filter(Aquifer == "122SNDS")
 SumTable_122HTRNN <-TableWLChanges %>% filter(Aquifer == "122HTRNN")
 SumTable_121TMIM <-TableWLChanges %>% filter(Aquifer == "121TMIM")
 SumTable_112NRSD <-TableWLChanges %>% filter(Aquifer == "112NRSD")
 print(xtable(TableWLChanges), type="html", file=paste0(MainDir,"tables/", "Inst_TableWLChanges", ".html"), include.rownames=FALSE)
 print(xtable(SumTable_112BSCNN), type="html", file=paste0(MainDir,"tables/", "Inst_SumTable_112BSCNN", ".html"), include.rownames=FALSE)
 print(xtable(SumTable_110SAQS), type="html", file=paste0(MainDir,"tables/", "Inst_SumTable_110SAQS", ".html"), include.rownames=FALSE)
 print(xtable(SumTable_122SNDS), type="html", file=paste0(MainDir,"tables/", "Inst_SumTable_122SNDS", ".html"), include.rownames=FALSE)
 print(xtable(SumTable_122HTRNN), type="html", file=paste0(MainDir,"tables/", "Inst_SumTable_122HTRNN", ".html"), include.rownames=FALSE)
 print(xtable(SumTable_121TMIM), type="html", file=paste0(MainDir,"tables/", "Inst_SumTable_121TMIM", ".html"), include.rownames=FALSE)
 print(xtable(SumTable_112NRSD), type="html", file=paste0(MainDir,"tables/", "Inst_SumTable_112NRSD", ".html"), include.rownames=FALSE)
 write.csv(TableWLChanges,paste(MainDir,"tables/Inst_SummaryTable",".csv", sep=''))


