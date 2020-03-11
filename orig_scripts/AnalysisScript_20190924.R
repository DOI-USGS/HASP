# ********************************************************************************************************
# R SCRIPT                                                                                               *
# SWStageRecorders_20190924.R                                                                            *
#                                                                                                        *
# Based on script by initially developed by Laura DeCicco, USGS Center for Integrated Data Analytics     *
# Modified by Scott Prinos, Hydrologist, USGS Carebean-Florida Water Science Center, Davie Florida       *
# September 27, 2019                                                                                     *
#                                                                                                        *
# Code was originally developed for groundwater level elevation monitoring sites so you will see a lot   * 
# of groundwater jargon in in the variable and file names. Then a varient was made for surface           *
# water so a few of names assume its working with surface water. Regardless of variable                  *
# names it can now be used with a lot  of different types of daily value data                            *
#                                                                                                        *
#*********************************************************************************************************

# Modifications made to code. Code has been broken into 18 functions, and one analytical script that processes
# data and sends it to functions, and recieves output from functions.
# Lines 51 - 62 are where you specify the main input parameters. 
# The script assumes you will create the folders "plots", "tables", and "HeaderFiles" 
# within your main directory "MainDir". This script is designed to work with the file AnalysisFunctions_20190923.R
# 

library(dataRetrieval)
library(xtable)
library(dplyr)
library(EnvStats)
library(lubridate)
library(openxlsx)
library(stringr)
require(lubridate)


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
# Change "MainDir" to whereever you want the output to go. Create a folder with that main directory 
# as well as subdirectories for the input files, plots, and tables.

MainDir <- "C:/Users/stprinos/Documents/stprinos_data/NewWebsite/" # Path for the main directory
TableWLChanges <-0 #  The variable TableWaterLevel will be in the que from the previous code runs and we do not want that. But a simple removal would throw an error code on the first code run. 
rm(TableWLChanges) #  This removes TableWLChanges and will not throw an error code, since the variable does exist given the previous line.

source(file.path(paste0(MainDir,"R_Scripts/FrequencyAnalysisScripts/AnalysisFunctions_20190923.R")))


# Initialize Variables
pCode <- c("72019")
statCd <- c("00001")
state <- "NE"
agencyCode<-"USGS"

 RecSites <- whatNWISsites(stateCd=state,parameterCd=pCode, siteStatus = "active")

# This is another option for pulling sites by county rather than state. These are the current counties shown
# in the existing water level and salanity analysis mapper. I will soon have to cut this to just 12086
# since the South Florida Water Management District cut cooperative support of this effort. 
 
# counties<-c("12011","12015","12021","12043","12051","12071","12085","12086","12087","12093","12099","12111")
# RecSites <- whatNWISsites(countyCd=counties, parameterCd=pCode, siteStatus = "active")

 
RecSites<-RecSites[,2:3]
names(RecSites)<-c("SiteID","SiteName")


# Get the site file information for each site
# CALL USGS FUNCTION readNWISsite
SiteInfo <- readNWISsite(RecSites$SiteID)

SiteInfo <- SiteInfo %>% 
  filter(agency_cd ==agencyCode)

GwSI_File_Rec <- SiteInfo %>% 
  select(dec_long_va, dec_lat_va, site_no, station_nm, huc_cd, site_tp_cd,
          alt_va, county_cd, land_net_ds, dec_coord_datum_cd)


# Save the results as an excel file
write.xlsx(GwSI_File_Rec,paste(MainDir,"tables/GWSI_File.xlsx", sep=''))
write.csv(GwSI_File_Rec,paste(MainDir,"tables/GWSI_File.csv", sep=''))



# Create the list of sites
Usites<-RecSites$SiteID


#
# END INTIAL SET UP
#--------------------------------------------------------------------------------------------------
#
#__________________________________________________________________________________________________

#__________________________________________________________________________________________________
#**************************************************************************************************
#**************************                     ***************************************************
# *************************    BEGIN MAIN LOOP  ***************************************************
# *************************                     ***************************************************
#**************************************************************************************************
#
#
# Begin a for loop that goes through the file and makes the graphs and tables for each site.

for(current_site in Usites){  
  
 # for loop ends at line 942 - comment out for testing
 

  # assign the site number and site name from the current site of the loop and the site info 
  siteNumber<-current_site
  print(current_site)
 
  
    #*************************************************************************************
    # ********                FUNCTION GWLEVEL                                ************
    #*********            READ IN THE DAILY VALUE DATA                        ************
    #*********              REARRANGE IT FOR ANALYSIS                         ************
    #*************************************************************************************
  
        gwlevel<-getDVData(current_site, pCode, statCd)
        
        # Get the information concerning the statistic and variable. 
        summary(gwlevel)
        names(attributes(gwlevel))
        # Get site info this is where name used for the title comes from and type type come from.
        siteInfo <- attr(gwlevel, "siteInfo")
        TypeCurrentSite<-siteInfo$siteTypeCd
        # Get the info on the parameter code this is where the variable description used for the plot comes from.
        variableInfo <- attr(gwlevel, "variableInfo")
        # This gets the description of the statistic
        statisticInfo <- attr(gwlevel, "statisticInfo")
        current_site_info <- SiteInfo %>% filter(site_no == current_site)
        
        
        
        # This function pulls the daily value data for the given site, parameter, and statistic
        # It creates a table that includes the data as well as creating columns for the month,
        # day of the year, year, week of the year, and USGS abbreviation for the month
        # these columns are used later to break up the data for analysis by month, week, or 
        # day of year. 
        OrigNames<-names(gwlevel)
       if(ncol(gwlevel)>11){
         publishedColumns<- gwlevel[,grepl("PUBLISHED",names(gwlevel))]
         codeColumns<-publishedColumns[,grepl("cd",names(publishedColumns))] 
         valueColumns<-publishedColumns[,!grepl("cd",names(publishedColumns))]
         dataDescriptors<-names(valueColumns)
         multipleDatasets<-TRUE
         }else if(ncol(gwlevel)<1){
           dataDescriptors<-"Empty"
           multipleDatasets<-FALSE
         } else{
         dataDescriptors<-OrigNames[4]
         multipleDatasets<-FALSE
       }
       
      #**************************************************************************************************
      #**************************                       *************************************************
      # *************************    BEGIN SECOND LOOP  *************************************************
      # *************************                       *************************************************
      #**************************************************************************************************
      #     
       # This is a for loop that cycles through all the 
       # steps for each data set (i.e. data descriptor) for a given site.
       # So if for example there are upstream and downstream data sets this for loop
       # will cycle through the steps to analyze both data sets.
        
       for (i in dataDescriptors){
         descritpText<-i
         if(multipleDatasets == TRUE){
           cdText<-paste0(descritpText,"_cd")
           curvalueColumn<-valueColumns[,grepl(descritpText,names(valueColumns))]
           curcodeColumn<-codeColumns[,grepl(descritpText,names(codeColumns))]
           gwlevel<-data.frame(cbind(gwlevel[,1:3], curvalueColumn, curcodeColumn, gwlevel$month,gwlevel$doy,gwlevel$year,gwlevel$NewDate,gwlevel$woy,gwlevel$AbbMonth))
           names(gwlevel)<-c("agency_cd","site_no","Date","value","status","month","doy","year","NewDate","woy","AbbMonth")
         }else if(dataDescriptors != "Empty"){
           names(gwlevel)<-names(gwlevel)<-c("agency_cd","site_no","Date","value","status","month","doy","year","NewDate","woy","AbbMonth")
         }
         
        
         # change the missing data symbol to na
         if(dataDescriptors != "Empty"){gwlevel$value[gwlevel$value==-999999.00]<-NA}
         
         #*************************************************************************************
         # ********                FUNCTION CHECKRESULTS                           ************    
         #*********              CHECK THE DAILY VALUE DATA                        ************
         #*********                                                                ************
         #*************************************************************************************
         
         checkResults <-checkDVData(gwlevel)
         
         # Ths fuction checks to see if: (1) there is any data for the site at all, (2)there is data for the
         # current water year and how much, (3) if there is complete data for the previous water year 
         # and how much, it also computes a number of variables used throughout the script including 
         # dateLastReading, startCurrentWY, currentYear, 
         
         
         # Test to see if there is data for a given site and if so perform analysis if not skip all the following steps 
         
         if(checkResults$IsAnyDataAvailable == "YES" && checkResults$IsThereDataCurrYr == "YES"){
           
          
           AvailableData<-whatNWISdata(siteNumber = current_site)
           siteName<-current_site_info$station_nm
           print(siteName)
           # Create the title for all plots, and plot files, and the path for plot files
           FullSiteName<-paste0("ID",siteNumber,"_",statisticInfo$statisticName,"_",variableInfo$variableCode, descritpText)
           path <- paste(MainDir, "plots/", FullSiteName, "_", "FrqA", ".png", sep='')
           
           
           # 
           # END PULLING DV AND IV DATA
           #---------------------------------------------------------------------------------------
           
           #**************************************************************************************
           # ********                FUNCTION COMPUTEDATES                       *****************
           #***********         COMPUTE THE START AND END DATES FOR PLOTS        *****************
           #**************************************************************************************
           
           computedDates<-computeDates(checkResults)
           
           # This function creates dates used for plotting including: startDate, startDate2yr, EndDate, startWeek, WoyLastReading,doyLastReading, DateLastMonth, DateLastWeek 
           # These dates are consolodated into the specified data frame - computedDates
           #**************************************************************************************
           
           # Given the dates computed by function "computeDates" create two datasets one with the last week of data, and another with the last month of record.
           LastWeeksRecord<-gwlevel%>% 
             filter(NewDate==computedDates$DateLastWeek)
           LastMonthsRecord <- gwlevel %>% filter(Date==computedDates$DateLastMonth)
           
           
           ###############################################################################
           #                                                                             #
           ###################     FUNCTION COMPUTESKTT     ##############################
           #                                                                             #
           ###############################################################################
           
           trendPeriods<-c(5,20)
           TrendTestResults<-computeSKTT(gwlevel, trendPeriods)
           
           # This fuction takes the daily value data proceeed by preceeding functions
           # and computes the Season Kedall Trend Test for twoor more trend periods
           # TrendPeriods must be a vector with at least two periods in years for the 
           # script to test. However if anything but 5 and 20 years is selected
           # there are changes that would need to be elsewhere 
           
           
           ###############################################################################
           #                                                                             #
           ###################     FUNCTION COMPUTEKTT     ##############################
           #                                                                             #
           ###############################################################################
           
           KTT_5Yr_TrendResults<-computeKTT(gwlevel, trendPeriods[1])
           KTT_20Yr_TrendResults<-computeKTT(gwlevel, trendPeriods[2])
           
           # This fuction takes the daily value data proceeed by preceeding functions
           # and computes the Kedall Trend Test for the specified trend trend period
           
           
           
           ###############################################################################
           #                                                                             #
           ###################     FUNCTION MARKERLOOKUP     #############################
           #                                                                             #
           ###############################################################################
           # Determine the appropriate marker based on the trend test results
           
           FiveYearSymbol<-markerLookup(TrendTestResults$trend[1])
           TwentyYearSymbol<-markerLookup(TrendTestResults$trend[2])
           
           ###############################################################################
           #                                                                             #
           ###################     FUNCTION ASSIGNLTSMBL     #############################
           #                                                                             #
           #*********   Based on Results of Trend Test Assign LT Trend Symbol ************
           #******************************************************************************
           
           finalLTSymbol<-assignLTSmbl(TrendTestResults)
           
           ###############################################################################
           #                                                                             #
           ###################   FUNCTION TRENDSANDCHANGES        ########################
           #                                                                             #
           #*********    Based on Results of trend tests and comparisons      ************
           #*********      assign trend symbols and compute changes           ************     
           #******************************************************************************
           
           
           changesAndSmybls<-trendsAndChanges(checkResults, LastWeeksRecord, LastMonthsRecord)
           
           
           # ____________________________________________________________________________________
           # *************************************************************************************
           # ************         BEGIN PERCENTILE RANKING           *****************************
           # ************         SET UP DATA FOR PLOTTING           *****************************
           # *************************************************************************************
           
           # Get the data for the current week of the year
           Data4CurrentWoy<-gwlevel%>% 
             filter(woy==computedDates$WoyLastReading)
           
           # Create a function to determine the percetile ranks of the water levels for the week of the year of the last data point,
           # and use this to determine the percentile rank of the last water level
           perc.rank <- function(x) trunc(rank(x, na.last = "keep"))/length(x)
           Data4CurrentWoy <- within(Data4CurrentWoy, xr <- perc.rank(Data4CurrentWoy$value))
           Data4LastReading<-Data4CurrentWoy %>%
             filter(Date == checkResults$dateLastReading)
           PercRankofLastReading<-Data4LastReading$xr[1]
           PercentileRanKLastRead <- round(PercRankofLastReading*100, digits = 1)
           
           # Get the last year of data
           lastYearofData<-gwlevel%>%
             filter(Date >= computedDates$startDate-1)
           # Select data that is approved and provisional
           lastYearofData_A<-lastYearofData%>%
             filter(grepl("A", lastYearofData$status))
           lastYearofData_P<-lastYearofData%>%
             filter(grepl("P", lastYearofData$status))
           # Create a column in each dataset that can be used to put it back in the original original order.
           lastYearofData$OrgOrder<-c(1:nrow(lastYearofData))
           if(nrow(lastYearofData_A) > 0){lastYearofData_A$OrgOrder<-c(1:nrow(lastYearofData_A))}
           if(nrow(lastYearofData_P) > 0){lastYearofData_P$OrgOrder<-c(1:nrow(lastYearofData_P))}
           
           
           # this with the last year of data. This puts NAs where there is no data. 
           fullYear<-as.data.frame(seq(as.Date(computedDates$startDate), as.Date(computedDates$EndDate), by="days"))
           fullYear_A<-as.data.frame(seq(as.Date(computedDates$startDate), as.Date(computedDates$EndDate), by="days"))
           fullYear_P<-as.data.frame(seq(as.Date(computedDates$startDate), as.Date(computedDates$EndDate), by="days"))
           colnames(fullYear)<-"Date"
           colnames(fullYear_A)<-"Date"
           colnames(fullYear_P)<-"Date"
           lastYearofData<-merge(fullYear, lastYearofData, by="Date", all.x=TRUE)
           lastYearofData_A<-merge(fullYear, lastYearofData_A, by="Date", all.x=TRUE)
           lastYearofData_P<-merge(fullYear, lastYearofData_P, by="Date", all.x=TRUE)
           
           
           
           ###############################################################################
           #                                                                             #
           ###################      FUNCTION COMPUTESTATS         ########################
           #                                                                             #
           ###############################################################################
           #******************************************************************************
           #*********    Based on Results of trend tests and comparisons      ************
           #*********      assign trend symbols and compute changes           ************     
           #******************************************************************************
           
           finalstatsOut<-computeStats(gwlevel, MainDir, FullSiteName)
           
           ###############################################################################
           #                                                                             #
           ###################      FUNCTION PRCNTLRANK         ########################
           #                                                                             #
           ###############################################################################
           #******************************************************************************
           #*********       Based on Results of frequency analysis            ************
           #*********       determine where the last reading ranks            ************
           #*********        and assign symbol color and result               ************
           #******************************************************************************
           
           prcntlRanking<-prcntlRank(finalstatsOut,Data4LastReading)
           
           StatsForWeek <- finalstatsOut %>% filter(`Week of the year` == Data4LastReading$woy)
           
           # *************************************************************************************
           # ************         END PERCENTILE RANKING             *****************************
           # ************      END SET UP DATA FOR PLOTTING          *****************************
           # *************************************************************************************
           # ____________________________________________________________________________________
           
           
           # CREATE TABLE WITH SUMMARY INFO FOR EACH SITE 
           
           SummaryWLChanges <- data.frame(current_site,siteName, current_site_info$dec_lat_va, current_site_info$dec_long_va, as.character(checkResults$dateLastReading), checkResults$valueLastReading, changesAndSmybls$WLChangeLastWeek,  paste0(prcntlRanking$SymbolCol,"-",changesAndSmybls$WLsymbol), changesAndSmybls$WLChangeLastMonth, paste0(prcntlRanking$SymbolCol,"-",changesAndSmybls$WLsymbolM), prcntlRanking$ColRGB, prcntlRanking$WLfreqResult, PercentileRanKLastRead, StatsForWeek$`Number of years`, checkResults$currentnessOfData, TrendTestResults$trend[1],paste0(prcntlRanking$SymbolCol,"-",FiveYearSymbol),TrendTestResults$trend[2],paste0(prcntlRanking$SymbolCol,"-",TwentyYearSymbol),paste0(prcntlRanking$SymbolCol,"-",finalLTSymbol),current_site_info$alt_datum_cd)
           
           if(exists("TableWLChanges")) {TableWLChanges<-rbind.data.frame(TableWLChanges, SummaryWLChanges)} else {TableWLChanges<-SummaryWLChanges}
           
           # ___________________________________________________________________________________________________________________
           #
           # ***************************  REARRAGE QUANTILES FOR MAKING INTO BOX PLOTS ******************************************
           #
           # Set up the limits for the quantile plots. Min and Max of daily values are not used because for the test
           # well the max was much larger than the quantiles.
           ymax<-max(gwlevel$value, na.rm=TRUE)
           ymin<-min(gwlevel$value, na.rm=TRUE)
           Rangefreq<-ymax-ymin
           plotrange<-abs(ymax-ymin)
           ymax<-ymax+plotrange*0.1
           ymin<-ymin-plotrange*0.1
           yLimits <- c(ymin, ymax)
           
           
           # Grab only the quantiles from finalStats
           
           quantData <- finalstatsOut[,c(-1,-9)]
           
           plotData <- quantData
           
           # This block of code converts the quantiles into bar segments that stack on top of eachother to form the 
           # frequency analysis on a week by week basis. Together all the segments add up to the maxMed
           for(i in 2:ncol(plotData)){
             plotData[,i] <- quantData[,i] - quantData[,i-1]
           }
           
           # I think this block of code determins the width of each bar, and adds it as a new column.
           # below I revise this code to the number of days in a week.
           
           
           #****************************************************************************************
           # If there is no data for the previous year there is no reason to compute these variables
           #****************************************************************************************
           if(checkResults$IsThereDataPrevYr == "YES"){
             
             plotData[,i+1] <- rep(7,52) 
             stats<-t(as.matrix(plotData))
             
             
             
             # rearrange the statistics to be in order of data for the last year
             if(computedDates$startWeek != 1){
               weekstoplot <- c((computedDates$startWeek+1):ncol(stats),1:computedDates$startWeek)
             } else {
               weekstoplot <- 1:ncol(stats)
             }
             
             stats <- stats[,weekstoplot]
             quantData <- quantData[weekstoplot,]
             
             # Create a dataframe that has the heights for months, the month abbreviation, and the month widths
             MonthInfo<-rbind(
               rep(yLimits[2], 12),
               
               c("Jan.","Feb.","Mar.", "Apr.", "May","June","July","Aug.","Sept.","Oct.", "Nov.", "Dec."),
               c(31,28.25,31, 30, 31, 30, 31, 31, 30, 31, 30, 31)/7.01
             )
             
             # rearrange the month info to be in same order as the data and stats for the last year
             if(computedDates$startMonth != 1){
               monthstoplot <- c((computedDates$startMonth):ncol(MonthInfo),1:computedDates$startMonth-1)
             } else {
               monthstoplot <- 1:ncol(MonthInfo)
             }
             
             MonthInfo<-MonthInfo[,monthstoplot]
             
             # a different MonthInfo to try to overlay months with limits of 
             MonthInfo2<-rbind(
               rep(yLimits[2], 12),
               rep(yLimits[1], 12)
             )
             MonthInfo3<-rbind(
               MonthInfo2[1,],
               MonthInfo2[2,]-MonthInfo2[1,]
               
             )
             
             MonthInfo3<-MonthInfo3[,monthstoplot]
             
           }
           
           
           # ____________________________________________________________________________________________________
           # ****************************************************************************************************
           # ************************** MAKE DATE OF GRAPH TEXT  ********************************************
           # ****************************************************************************************************
           DateGraphText<-paste0("Graph created ", checkResults$Cdate)
           
           # ____________________________________________________________________________________________________
           # ****************************************************************************************************
           # **************************          FUNCTION            ********************************************
           # ************************** MAKE FREQUENCY ANALYSIS PLOT ********************************************
           # ****************************************************************************************************
           
           if(checkResults$IsThereDataPrevYr == "YES"){
             xlimDays<-FreqAnlysPlot(lastYearofData, computedDates$startDate, computedDates$EndDate, path, variableInfo, siteName, siteNumber, quantData, lastYearofData_A, lastYearofData_P, yLimits, MonthInfo3, MonthInfo, statisticInfo$statisticName, agencyCode)
           }
           
           #
           
           # ****************************************************************************************************
           # **************************          FUNCTION            ********************************************
           # ******************** MAKE 2 YEAR PLOT AND ASSOCIATED FILES  ****************************************
           # ****************************************************************************************************
           
           if(checkResults$IsThereDataPrevYr == "YES"){
             POYStatsAndData<-Make2yrPlot(gwlevel, computedDates$startDate2yr, computedDates$EndDate, yLimits, computedDates$startMonth, MainDir, FullSiteName, siteName, siteNumber, xlimDays,statisticInfo$statisticName,agencyCode)
           }
           
           # The fuction "Make2yrPlot" creates the 2 year graphs, the block of text that goes next to the 2 yr graphs
           # in the website, and the table of daily statistics that goes next to the 2 yr graphs in the website
           # It prints all these results to the specified folders. 
           
           
           #____________________________________________________________________________________________________
           # ****************************************************************************************************
           # *********************** MAKE LONG TERM PLOT  *******************************************************
           # ****************************************************************************************************
           
         
             MakeLTPlot(gwlevel, MainDir, FullSiteName, siteName, siteNumber, TrendTestResults, KTT_5Yr_TrendResults, KTT_20Yr_TrendResults, checkResults$dateLastReading,statisticInfo$statisticName, agencyCode)
           
           
           
           # This function makes the long term plot and prints it out to a folder. 
           
           #____________________________________________________________________________________________________
           # ****************************************************************************************************
           # ***********************     MAKE TABLES      *******************************************************
           # ****************************************************************************************************
           
           dailyDataSummary<-MakeTables(gwlevel, MainDir, FullSiteName, current_site_info, TrendTestResults)
           
           # This function creates the Daily Value, Long Term Trend, and Daily Data Summary tables. It prints them
           # and returns "dailyDataSummary"
           
           #____________________________________________________________________________________________________
           # ****************************************************************************************************
           # ***********************     MAKE TEXT     *******************************************************
           # ****************************************************************************************************
           
           if(checkResults$IsThereDataPrevYr == "YES"){
             MakeText(statisticInfo, current_site_info, POYStatsAndData, checkResults$dateLastReading, siteNumber,siteName, checkResults$valueLastReading, prcntlRanking$WLfreqResult, Data4LastReading, dailyDataSummary, variableInfo, TypeCurrentSite, FullSiteName)
           }else{
             MakeText2(statisticInfo, current_site_info, checkResults$dateLastReading, siteNumber,siteName, checkResults$valueLastReading,  Data4LastReading, dailyDataSummary,variableInfo, TypeCurrentSite, FullSiteName)
           }
           
           
           # This function creates the text for the station pages. Including site information, and summaries of daily data, 
           # and the frequency analysis
           
           
         }
         
       }
       
      

 }
  

  
  
data.frame(TableWLChanges, row.names = NULL)




names(TableWLChanges) <-c("site_no","site_nm", "dec_lat_va","dec_long_va", "wl_date", "wl",  "week_change", "week_symbol", "month_change", "month_symbol", "rgb_color","freq", "perc", "years", "days_of_last_value", "trend_5year","FiveYearSymbol","trend_20year","TwentyYearSymbol", "long_term_symbol","Vertical_Datum")




write.csv(TableWLChanges,paste(MainDir,"tables/Daily_",statCd, "_",pCode, "_Summary",".csv", sep=''))
print(xtable(TableWLChanges), type="html", file=paste0(MainDir,"tables/Daily_",statCd, "_",pCode, "_Summary", ".html"), include.rownames=FALSE)



