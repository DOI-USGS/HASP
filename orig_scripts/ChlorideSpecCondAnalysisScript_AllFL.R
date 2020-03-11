#
# SCRIPT - CHLORIDESPECCONDANALYSISSCRIPT_AllFL.R
#
# created by Scott Prinos, 5/10/2016, 
# Modified by Lindsay Carr, 6/15/2016, 
# Modified by Scott Prinos, 07/31/2018
# 
#
# Modification made: Added a little subroutine to near the begining of the script (lines 64-72) to pull all active USGS sites  
# and then test to make sure that these sites have choride data (99220) that has been collected within the last two years. 
# This listing of sites then becomes the site list used by the main script.

# 
#
# __________________________________________________________________________________________________
#
# SETTING UP THE ENVIROMENT FOR THE SCRIPT TO RUN
# ___________________________________________________________________________________________________
#
# 1 - Create a directoring with the following subdirectories:
#   - R_scripts
#   - InputFiles
#   - plots
#   - tables
# 
# 2 - Put the files "markerLookupTable.csv" and "ActiveChloride.csv" in the InputFiles directory
# 3 - Put the scripts "ChlorideSpecCondAnalysisScript_mod8.R", "helperfunctions_mod5.R" and 
#     "trendAnalysisFunctions_mod6.R" in the "R_Scripts" subdirectory
# 4 - Provide the path for the main directory you have created in line 63 of this script 
#     for the variable "MainDir"
# __________________________________________________________________________________________________
# 
# 5 - Make sure all the packages are loaded and that the libraries are available. This does not have to be
#     done everytime the script is rerun.
#
# Do not run the install package sections every time. But if you need to you can uncomment it and use it.
install.packages("dataRetrieval")
install.packages("xtable")
install.packages("xlsx")
install.packages("dplyr")
install.packages("EnvStats")
install.packages("htmlTable")
install.packages("lubridate")
library(htmlTable)
library(dataRetrieval)
library(xtable)
library(xlsx)
library(dplyr)
library(EnvStats)
library(lubridate)
#
# _________________________________________________________________________________________________
# 
# BEGINING OF THE SCRIPT
#

MainDir <- "C:/Users/stprinos/Documents/stprinos_data/NewWebsite/" # Path for the main directory see STEP 2

# Setup seasonal trend analysis
source(file.path(paste(MainDir,"R_Scripts/trendAnalysisFunctions_mod6.R",sep='')))
source(file.path(paste(MainDir,"R_Scripts/helperfunctions_mod5.R",sep='')))
trendResults <- data.frame()
ClvsSC_all <- data.frame()


# Identify the sites that have had chloride sample collected during the last 2 data
CLTestsites <- whatNWISsites(stateCd="FL",parameterCd="99220", siteStatus = "active")
CLdataTest <- readNWISqw(CLTestsites$site_no, "99220")
CurrentYear <- as.POSIXlt(today())$year + 1900
CLdataTest$sample_yr <- as.POSIXlt(CLdataTest$sample_dt)$year + 1900
CurrentWatSampDat<-CLdataTest%>% 
  filter(sample_yr > CurrentYear -2)%>% 
  filter(parm_cd == "99220")
SitesWRecentData <- unique(CurrentWatSampDat$site_no)


# Get the site file information for each site
# CALL USGS FUNCTION readNWISsite
SiteInfo <- readNWISsite(SitesWRecentData) 
GwSI_File <- SiteInfo %>% 
  select(dec_long_va, dec_lat_va, site_no, station_nm, well_depth_va, hole_depth_va,
         aqfr_cd, alt_va, county_cd, land_net_ds, dec_coord_datum_cd)
write.xlsx(GwSI_File,paste(MainDir,"tables/GWSI_File.xlsx", sep=''))

# This uses a USGS data retreival function to get the water quality data
ParameterCodes <- c("00095","90095","00940","99220", "72020")
# CALL USGS FUNCTION readNWISqw
RawWatSampDat <- readNWISqw(SitesWRecentData, ParameterCodes) 
## NEW CODE TO GET DATES IN THE RIGHT FORMAT ********************
## remove observations with dates that are not formatted properly
## then convert to actual dates
RawWatSampDat <- RawWatSampDat %>% 
  filter(grepl("\\d{4}-\\d{2}-\\d{2}", sample_dt)) %>% 
  mutate(sample_dt = as.Date(sample_dt, format = "%Y-%m-%d"))


###################

## change observations with dates that are missing the day value to have a day
## then convert to actual dates
RawWatSampDat <- RawWatSampDat %>% 
  rowwise %>% 
  mutate(sample_dt = 
           if(grepl("\\d{4}-\\d{2}-\\d{2}", sample_dt)){
             as.Date(sample_dt, format = "%Y-%m-%d")
           } else {
             as.Date(paste0(sample_dt, "-01"))
           }
  ) %>% 
  ungroup

## END OF NEW CODE##############################

# Get just the columns of interest
## WARNING: ignoring censored values by not keeping the remark_cd column ##
WatSampDat <- RawWatSampDat %>% select(site_no, sample_dt, parm_cd, result_va) 

# Most of the 00940 (filtered) chloride sample results are probably from unfiltered (99220) so switch the parameter code to 99220 to combine both
WatSampDat <- WatSampDat %>% 
  mutate(parm_cd = ifelse(parm_cd == "00940", "99220", parm_cd))

# Most of the 00095 (field) specific conductance sample results are probably from lab analyzed (90095) so switch the parameter code to 90095 to combine both
WatSampDat <- WatSampDat %>% 
  mutate(parm_cd = ifelse(parm_cd == "00095", "90095", parm_cd))

Usites <- unique(WatSampDat$site_no) # Get the unique site IDs from the file

# Loop through the WatSampDat file parsing it by station ID and then by parameter code
 for(current_site in Usites){
# current_site <- '254457080160301'
  WellInfo <- SiteInfo %>% 
    filter(site_no == current_site) 
  FullSiteName <- paste(WellInfo$station_nm, WellInfo$site_no)
   print(FullSiteName)
  
  SiteSampDat <- WatSampDat %>% filter(site_no == current_site)

  # Pull chloride data
  Cldata <- SiteSampDat %>% 
    filter(parm_cd == "99220") %>% 
    filter(!duplicated(sample_dt)) 
  
  # Pull conductivity data
  SCdata <- SiteSampDat %>% 
    filter(parm_cd == "90095") %>% 
    filter(!duplicated(sample_dt))

  # CREATE PLOTS AND TABLES OF SPECIFIC CONDUCTANCE VERSUS CHLORIDE
  Plotdata <- merge(Cldata, SCdata, by=c("sample_dt", 'site_no')) %>% 
    rename(Date = sample_dt, `Station ID` = site_no,
           `Chloride concentration` = result_va.x, 
           `Specific conductance` = result_va.y)# %>% 
    # mutate(Date = as.Date(Date))
  #
  # CALL FUNTION SCCLPLOTTABLE
  ScClplot_and_table <- ScClPlotTable(Plotdata =  Plotdata, MainDir = MainDir, 
                                      plotTitle = FullSiteName)
  ScCltable <- ScClplot_and_table$table
  
  # Create a plot and table of all the chloride vs specific conductance
  ClvsSC_all <- rbind(ClvsSC_all, ScCltable)
  # CALL FUNTION SCCLPLOTTABLE
  ClvsSC_allplot_and_table <- ScClPlotTable(Plotdata =  ClvsSC_all, MainDir = MainDir, 
                                            plotTitle = "Specific conductance vesus chloride concentration")
  
  # Begin a loop to process data for each site by parameter code
  
  for(pc in ParameterCodes){
      print(pc)
    
   
     
    dat <- SiteSampDat %>% filter(parm_cd == pc)
    
    
    if(length(dat$sample_dt) >0 & length(dat$result_va)>0){
  
      if(pc == "99220"){
        # CALL FUCTION trendAnalysis 
        trendResults_i <- trendAnalysis(siteID = WellInfo$site_no, 
                                        stationName = WellInfo$station_nm, 
                                        clData_raw = dat,
                                        pctComplete = 0.8)
        # create a file with all of the trend analysis results
        trendResults <- rbind(trendResults, trendResults_i) 
        # Preserve the trend results for this site in its current form
        trendResults_i2 <-trendResults_i 
        # create different form of the trend results for this site which 
        # has no lines for results that do not have a slope
        trendResults_i <- trendResults_i %>% filter(!is.na(slope)) 
      } 
      
      # CALL FUNCTION chlorideTimeseriesTable
      clTimeseries <- chlorideTimeseriesTable(MainDir, FullSiteName, dat, trendResults_i, trendResults_i2, pc)
      
    }
   
  }
  
 }

# Save the trend results file to the tables directory 
write.xlsx(trendResults,paste(MainDir,"tables/trendResults.xlsx", sep=''))

# Save the file of all the chloride vs specific conductance
write.xlsx(ClvsSC_all,paste(MainDir,"tables/SCvsCL.xlsx", sep=''))

#colmnLables<- c("Latitude, in decimal degrees", "Longitude, in decimal degrees", "Datum", "Local aquifer code","National aquifer code", "Site identifier", "Site name", "Date of last chloride sample", "Chloride concentration, in milligrams per liter", "Trend direction", "Map symbol" )
 colmnLables<- c("dec_lat_va","dec_long_va","dec_coord_datum_cd","aqfr_cd","nat_aqfr_cd","site_no","stationName","lastSampleDate","chloride","FiveyrTrend", "FiveyrSymbol", "TwentyYrTrend", "TwentyYrSymbol", "OverallTrend", "OverallSymbol")

# This block of code has been modified to provide the five- and twenty-year trend test results.
 
fiveYearResults<-mapTrends <- trendResults %>%
  filter(trendType=="Five Year")%>%
  select(trend, colorMarker)
names(fiveYearResults)<-c("FiveyrTrend", "FiveyrSymbol")
TwentyYearResults<-mapTrends <- trendResults %>%
  filter(trendType=="Twenty Year")%>%
  select(trend, colorMarker)
names(TwentyYearResults)<-c("TwentyYrTrend", "TwentyYrSymbol")
OverallResults <- trendResults %>%
  filter(trendType=="Overall")%>%
  select(siteID, stationName, lastSampleDate, chloride, trend, colorMarker)
  names(OverallResults)<-c("siteID", "stationName", "lastSampleDate", "chloride","OverallTrend", "OverallSymbol")
# create the file that is going to be used by the website for showing trends in chloride on a map 
  mapTrends <- cbind(OverallResults, fiveYearResults, TwentyYearResults)
mapTrends<-merge(SiteInfo, mapTrends, by.x = "site_no", by.y = "siteID")%>%
  select(dec_lat_va,dec_long_va, dec_coord_datum_cd,aqfr_cd, nat_aqfr_cd, site_no, 
         stationName, lastSampleDate, chloride, FiveyrTrend, FiveyrSymbol, TwentyYrTrend, TwentyYrSymbol, OverallTrend, OverallSymbol )


# save a file with map trends for surface water sites only - no aquifer
#mapTrendNA<-mapTrends[is.na(mapTrends$aqfr_cd),]
mapTrendNA <- mapTrends %>%
  filter(is.na(aqfr_cd))
names(mapTrendNA)<- colmnLables
write.csv(mapTrendNA,paste(MainDir,"tables/MapTrend", "NA",".csv", sep=''))


# Save files for the map trends by aquifer. 
# This is used so that map trends can be shown on an aquifer 
# by aquifer basis.
#

mapTrends2 <- mapTrends %>%
  filter(!is.na(aqfr_cd))

aquifers<-unique(as.character(mapTrends$aqfr_cd))
for (i in aquifers){
  # print(i)
  mapTrendtemp<-mapTrends2 %>%
    filter(aqfr_cd == i)
  names(mapTrendtemp)<- colmnLables
  write.csv(mapTrendtemp,paste(MainDir,"tables/MapTrend", i,".csv", sep=''))
}

# Save the mapTrends file as a csv file
names(mapTrends)<- colmnLables
write.csv(mapTrends,paste(MainDir,"tables/MapTrend.csv", sep=''))

write.csv(SiteInfo,paste(MainDir,"tables/SiteInfo",".csv", sep=''))

