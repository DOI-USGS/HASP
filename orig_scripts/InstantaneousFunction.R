# ********************************************************************************************************
# INSTANTANEOUS DATA FUCTION                                                                             *
#                                                                                                        *
#*********************************************************************************************************

# This scipt has been crafted as a stand alone function for processing of instananeous data.
# This could be run on an ongoing basis to keep the instaneous data in the mapper up to date.
# It is designed to pull all the instanteious data for a given state however commented out for now
# is a block of code that could pull data by county instead. It can also run for how ever many days 
# of instantaneous data are desired. In theory it could be imbedded in the daily script but if so 
# the data frame SiteResults would have to be passed to the main script and consolodated there.
# 
# This script assumes a specific directory structure where "plots", "tables", and "HeaderFiles" are
# all folders listed in the main folder which is listed in the path "MainDir".

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



# INPUT VALUES FOR THE FUNCTION

pCode <- c("00065")
state <- "IL"
agencyCode<-"USGS"
PlotLengthDays<-30
MainDir <- "C:/Users/stprinos/Documents/stprinos_data/NewWebsite/" # Path for the main directory



# ____________________________________________________________________________________________________
# ****************************************************************************************************
# ****************                FUNCTION makeUVPLOT                      ***************************
# ****************                                                         ***************************
# ****************       Makes text blocks for station pages               ***************************
# ****************            MaKes unit value plots                       ***************************
# ****************       Makes an output table summerizing results         ***************************
# ****************************************************************************************************

MakeUVPlot <- function(pCode, state, PlotLengthDays, MainDir, agencyCode){
  
  print(pCode)
  
  # INITIALIZE OTHER VARIABLES USED. Once this is a function this part may not be needed since it would not be input
  
 
  
  # PULL DATA USED IN FUNCTION, GIVEN THE INPUT VARIABLES
  
  RecSites <- whatNWISsites(stateCd=state,parameterCd=pCode,  siteStatus = "active")
  
  RecSites<-RecSites %>%
    filter(agency_cd == agencyCode)
  
  # OPTION 2 - this is another option for pulling sites by county rather than state. These are the current counties shown
  # in the existing water level and salanity analysis mapper. I will soon have to cut this to just 12086
  # since the South Florida Water Management District cut cooperative support of this effort. 
  
  # counties<-c("12011","12015","12021","12043","12051","12071","12085","12086","12087","12093","12099","12111")
  # RecSites <- whatNWISsites(countyCd=counties, parameterCd=pCode, siteStatus = "active")
  
  RecSites<-RecSites[,2:3]
  names(RecSites)<-c("SiteID","SiteName")
  
  # Create the list of sites
  Usites<-RecSites$SiteID
  #Usites<-"02327031" # This can be used for testing just one site delete when converted to fuction
  
  
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
    
    # assign the site number and site name from the current site of the loop and the site info 
    siteNumber<-current_site
    print(current_site)
    
    
    
    #*************************************************************************************
    #*********            READ IN THE DAILY VALUE DATA                        ************
    #*********              REARRANGE IT FOR ANALYSIS                         ************
    #*************************************************************************************
    currentDate<-as.Date(Sys.time()) 
    OneMonthAgo<-as.Date(Sys.time()-(PlotLengthDays*86400))  
    uvdata<-readNWISuv(current_site, pCode, startDate = OneMonthAgo, endDate = currentDate)
    
    #**************************************************************************************************
    #**************************                       *************************************************
    # *************************    BEGIN SECOND LOOP  *************************************************
    # *************************                       *************************************************
    #**************************************************************************************************
    
    
    
    if(length(uvdata > 0)){
      
      
      # Get site info, statistic info, and variable info.
      SiteInfo <- readNWISsite(current_site)
      SiteInfo<-SiteInfo %>%
        filter(agency_cd == agencyCode)
      
      variableInfo <- attr(uvdata, "variableInfo")
      # This gets the description of the statistic
      
      siteName<-SiteInfo$station_nm
      uvdata<-renameNWISColumns(uvdata)
      
      OrigNames<-names(uvdata)
      if(ncol(uvdata)>6){
        publishedColumns<- uvdata[,grepl("PUBLISHED",names(uvdata))]
        codeColumns<-publishedColumns[,grepl("cd",names(publishedColumns))] 
        valueColumns<-publishedColumns[,!grepl("cd",names(publishedColumns))]
        dataDescriptors<-names(valueColumns)
        multipleDatasets<-TRUE
      }else if(ncol(uvdata)<1){
        dataDescriptors<-"Empty"
        multipleDatasets<-FALSE
      } else{
        dataDescriptors<-OrigNames[4]
        multipleDatasets<-FALSE
      }
      
      #**************************************************************************************************
      #**************************                       *************************************************
      # *************************    BEGIN THIRD LOOP   *************************************************
      # *************************                       *************************************************
      #**************************************************************************************************
      #     
      # This is a for loop that cycles through all the 
      # steps for each data set (i.e. data descriptor) for a given site.
      # So if for example there are upstream and downstream data sets this for loop
      # will cycle through the steps to analyze both data sets.
      
      for (i in dataDescriptors){
        
        # Set up a dataframe to hold results if any
        
        
        descritpText<-i
        #descritpText<-dataDescriptors[1]
        FullSiteName<-paste0("ID",siteNumber,"_UV_",variableInfo$variableCode, descritpText)
        
        if(multipleDatasets == TRUE){
          cdText<-paste0(descritpText,"_cd")
          curvalueColumn<-valueColumns[,grepl(descritpText,names(valueColumns))]
          curcodeColumn<-codeColumns[,grepl(descritpText,names(codeColumns))]
          uvdata<-data.frame(cbind(uvdata[,1:3], curvalueColumn, curcodeColumn, uvdata$tz_cd ))
          names(uvdata)<-c("agency_cd","site_no","dateTime","value","status","tz_cd")
        }else if(dataDescriptors != "Empty"){
          names(uvdata)<-names(uvdata)<-c("agency_cd","site_no","dateTime","value","status","tz_cd")
        }
        
        
        # Make sure that all the data is not missing
        testForSuffData<-na.omit(uvdata$value)
        
        
        
        
        
        if(length(testForSuffData)>0){ 
          
          
          dtLastUV<-uvdata$dateTime[length(uvdata$dateTime)]
          dtPrevUV<-uvdata$dateTime[length(uvdata$dateTime)-1]
          timeDiff<-difftime(dtLastUV,dtPrevUV,units="mins")
          LastUV<-uvdata[length(uvdata$dateTime),4]
          prevUV<-uvdata[length(uvdata$dateTime)-1,4]
          valueDiff<-round(LastUV-prevUV, digits = 2)
          
          if(is.na(LastUV)||is.na(prevUV)){
            valueChange<-"missing readings"
            symbChange<-"square"
          } else if (valueDiff > 0){
            valueChange<-"increased"
            symbChange<-"up arrow"
          } else if (valueDiff < 0){
            valueChange<-"decreased"
            symbChange<-"down arrow"
          } else if (valueDiff == 0){
            valueChange<-"stayed the same"
            symbChange<-"circle"
          }
          
          # Get the time zone of the computer system
          tZone<-Sys.timezone()
          
          # Convert the time of the last value to the local time based on the computer system time zone
          tzLastUV<-as.character(uvdata$tz_cd[length(uvdata$dateTime)])
          dtLastUV <- as.character(dtLastUV)  
          cnvDtLastUV <- as.POSIXct(dtLastUV, tz=tzLastUV)
          attributes(cnvDtLastUV)$tzone <- tZone
          
          # Breack the resulting date time into pieces so it can be used more easily
          dateLastUV <- format(cnvDtLastUV,"%m-%d-%Y")
          timeLastUV<-format(cnvDtLastUV,"%H:%M:%S")
          tzLastUV2<-as.character(format(cnvDtLastUV,"%Z"))
          
          timeSinceLast<-difftime(as.POSIXct(Sys.time(), tz=tZone), cnvDtLastUV, units = "hours" )  
          
          if (valueChange == "missing readings"){
            Text1<-paste0("The most recent records of ", tolower(variableInfo$variableDescription), " are unavailable for analysis.")
            cat(Text1)
            sink(paste0(MainDir,"HeaderFiles/",FullSiteName, "_", "UV", ".txt"))
            cat(Text1)
            cat("\n")
            sink()
          } else {
            
            Text1<-paste0("The last record of ", tolower(variableInfo$variableDescription), " was recorded at ",timeLastUV," ",tzLastUV2, " on ", dateLastUV,  " and was ",LastUV," ",variableInfo$unit,".")
            
            
            Text2<-paste("This was recorded", round(timeSinceLast, digits = 2),"hours ago.")
            
            
            if(valueChange == "stayed the same"){Text3<-paste0(variableInfo$variableDescription," had ",valueChange," since the previous reading." ) }
            
            
            if(valueChange == "increased" || valueChange == "decreased" ){
              Text3<-paste0(variableInfo$variableDescription," had ",valueChange," by ",valueDiff," ",variableInfo$unit," in the ",timeDiff, " minutes since the previous reading." )
              
              
            }
            
            
            cat(Text1)
            sink(paste0(MainDir,"HeaderFiles/",FullSiteName, "_", "UV", ".txt"))
            cat(Text1)
            cat("\n")
            cat(Text2)
            cat("\n")
            cat(Text3)
            cat("\n")
            sink()
            
          }
          
          
          
          
          # ____________________________________________________________________________________________________
          # ****************************************************************************************************
          # ****************                   Make UVPLOT                           ***************************
          # ****************                                                         ***************************
          # ****************              Makes a one week plot                      ***************************
          # ****************************************************************************************************
          
          
          #
          # Determine the maximums and minimuns of the continous and instantaneous data and determine plot limits. First make sure there are no NAs
          ContWLData<-na.omit(uvdata$value)
          maxCont<-max(ContWLData)
          minCont<-min(ContWLData)
          diffMaxMin<-maxCont - minCont
          if(diffMaxMin < 1){
            ymaxLT<-maxCont+0.5
            yminLT<-minCont-0.5
            ylimLT<-c(yminLT, ymaxLT)
          } else {
            ymaxLT<-maxCont+0.1
            yminLT<-minCont-0.1
            ylimLT<-c(yminLT, ymaxLT)
          }
          
          
          
          UVpath <- paste(MainDir, "plots/", FullSiteName, "_", "UV", ".png", sep='')
          
          # The following block of code selects the approved data
          
          LTPlotData_A<-uvdata%>%
            select(value, dateTime, status)%>%
            filter(grepl("A", uvdata$status))
          
          # The following block of code selects the provisional data
          
          LTPlotData_P<-uvdata%>%
            select(value, dateTime, status)%>%
            filter(grepl("P", uvdata$status))
          
          
          
          #Check to see which starts first provisional or approved data. Determine the earliest start date .
          if (nrow(LTPlotData_A)==0){plotStartDate<-LTPlotData_P[1,2]} else{plotStartDate<-LTPlotData_A[1,2]} 
          
          #Check to see which ends last Provisional or Approved data. Determine the latest end date .
          if (nrow(LTPlotData_P)==0){plotEndDate<-LTPlotData_A[length(LTPlotData_A[,2]),2]} else{plotEndDate<-LTPlotData_P[length(LTPlotData_P[,2]),2]} 
          
          # Determine the limits for the unit value plot
          
          xlimLT <-c(as.integer(plotStartDate), as.integer(plotEndDate))
          
          
          
          
          
          # In some instances the instantaneous data starts before the continous data, and this instantaneous data was not showing up in
          # in the long term plot. The following block of code tests to see if the instantaneous data started first.
          
          
          
          
          
          DateGraphText<-paste0("Graph created ", today())
          
          png(file=UVpath, width = 700, height = 600)
          par(oma = c(4, 1, 1, 1))
          par(mar = c(6, 5, 3, 1)+0.1)
          #par(mar = c(6, 4, 3, 2)+0.1) # original margins
          # New plot code with x and y limits
          ylable<-paste(toupper(variableInfo$variableDescription))
          wr.lap <- wrap.labels(ylable, 50)
          fullID<-paste(agencyCode, siteNumber)
          
          plot(uvdata$dateTime,uvdata$value, type = "l", col="white", ylim = ylimLT, xlim = xlimLT, main=paste(c(siteName, fullID)),
               xlab = "Date", ylab = wr.lap, tck=0.05)
          if(nrow(LTPlotData_A)>0){lines(LTPlotData_A$dateTime,LTPlotData_A$value, type = "l", col="blue")}
          if(nrow(LTPlotData_P)>0){lines(LTPlotData_P$dateTime,LTPlotData_P$value, type = "l", col="red")}
          
          
          
          # finish up the graph
          axis(3, labels=FALSE, tck=0.025)
          axis(4, labels=FALSE, tck=0.025)
          mtext("U.S. Geological Survey", adj=0, col="dark green")
          mtext(DateGraphText, side=3, adj=1,col="black", cex=0.8)
          
          
          
          par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE, xpd=TRUE) # this sets the legend up
          legend("bottomright", 
                 c("EXPLANATION", "   Approved data (dashed where intermittent)", "   Provisional data"), 
                 lty=c(NA,1,1), 
                 pch=c(NA,NA,NA),
                 col=c("black", "blue", "red"),
                 lwd=c(1,1,1),
                 merge=FALSE,
                 xpd = TRUE, 
                 horiz = FALSE, 
                 inset = c(0, -0.22)
          )
          
          dev.off()
          
          # ____________________________________________________________________________________________________
          # ****************************************************************************************************
          # ****************              Make UV output table                       ***************************
          # ****************                                                         ***************************
          # ****************           Makes a table with UV info                    ***************************
          # ****************************************************************************************************
          #
          ResultSumary<-c(rep(NA,16))
          SiteResults<-data.frame(rbind(ResultSumary))
          colnames(SiteResults)<-c("Longitude", "Latitude", "Datum",  "SiteID", "SiteName", "Description", "Parameter", "DateLastRecord", "TimeLastRecord","tzLastRecord","ValueLastRecord",  "HowLongAgo", "ChangeinValue", "ChangeinTime", "ChangeDescription", "ChangeSymbol")
          
          SiteResults$Longitude<-as.character(SiteInfo$dec_long_va)  
          SiteResults$Latitude<-as.character(SiteInfo$dec_lat_va)
          SiteResults$Datum<-SiteInfo$dec_coord_datum_cd
          SiteResults$SiteID<-as.character(current_site)
          SiteResults$SiteName<-siteName
          SiteResults$Description<-descritpText
          SiteResults$Parameter<-variableInfo$variableDescription
          SiteResults$DateLastRecord <-as.character(dateLastUV)
          SiteResults$TimeLastRecord <-as.character(timeLastUV)
          SiteResults$tzLastRecord<-tzLastUV2
          SiteResults$ValueLastRecord<-LastUV
          SiteResults$ChangeinValue<-valueDiff
          SiteResults$ChangeinTime<-as.character(timeDiff)
          SiteResults$ChangeDescription<-valueChange
          SiteResults$ChangeSymbol<-symbChange
          SiteResults$HowLongAgo<-round(timeSinceLast, digits = 2)
          
          # Create an output table that has all the results
          
          if(exists("TableUVChanges")) {TableUVChanges<-rbind.data.frame(TableUVChanges, SiteResults)} else {TableUVChanges<-SiteResults}
          
          
          
          } else{ print(c("There is not sufficient data for site ", current_site))}
        
      }
      
      
    }else{ print(c("There is not sufficient data for site ", current_site))}
    
  }
  
  if(exists("TableUVChanges")){
    data.frame(TableUVChanges, row.names = NULL)
    write.csv(TableUVChanges,paste(MainDir,"tables/UVRecorders_SummaryTable","UV",pCode, ".csv", sep=''))
    print(xtable(TableUVChanges), type="html", file=paste0(MainDir,"tables/", "UVRecorders_SummaryTable","UV",pCode, ".html"), include.rownames=FALSE)
  }
  
  
  
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


###############################################################################
#                                                                             #
#                                CALL                                         #
###################      FUNCTION PRCNTLRANK         ########################
#                                                                             #
###############################################################################
#******************************************************************************
#*********       Based on Results of frequency analysis            ************
#*********       determine where the last reading ranks            ************
#*********        and assign symbol color and result               ************
#******************************************************************************

MakeUVPlot(pCode, state, PlotLengthDays, MainDir,agencyCode)




