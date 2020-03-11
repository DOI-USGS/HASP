
#
# FUNTION HELPER FUNCTIONS
#
# Created by Lindsay Carr - 6/15/2016, rewritten by Scott Prinos 8/24/2016, modified by Scott Prinos, 12/19/2016
# 
#
# These are fuctions that are called by the script TrendanalysisFunctions_mod5.R,
# 
#
#___________________________________________________________________________________
#
#***********************   FUNCTION getParmCodeDef **********************************
#
#___________________________________________________________________________________
#
# Function provides the text description for the parameter codes
#
getParmCodeDef <- function(pc){
  
  switch(pc, 
         "99220" = "Chloride concentration, in milligrams per liter",
         "90095" = "Specific conductance, in microsiemens per liter",
         "72020" = "Water level elevation, in feet"
         )
}

#___________________________________________________________________________________
#
#***********************   END FUNCTION getParmCodeDef ****************************
#
#___________________________________________________________________________________


#___________________________________________________________________________________
#
#***********************   FUNCTION ScCLPlotTable **********************************
#
#___________________________________________________________________________________
#
# Function creates the individual chloride versus specfic conductance plots for each site
# as well as the chloride versus specific conductance plot for all sites.

ScClPlotTable <- function(Plotdata, MainDir, plotTitle, allData = FALSE){
  
  
  # Specify the plot titles using the function getParmCodeDef
  
  Cltitle <- getParmCodeDef("99220")
  Sctitle <- getParmCodeDef("90095")
  
  #different column names depending on whether it is all the data (ClvsSC_all) or not
  Plotdata <- Plotdata %>% 
    select(Date, `Station ID`, 
           `Specific conductance` = matches("Specific conductance"), 
           `Chloride concentration` = matches("Chloride concentration"))
  
  
  
  # Create a path including plot name for each plot
  
  path <- paste(MainDir,"plots/", plotTitle,"_SCvsCL",".png",sep='')
  Tablepath <- paste0(MainDir,"tables/", plotTitle, "_SCvsCL", ".html")
  
  # Create SC versus chloride plots of the data by station ID and parameter code.

  # Make sure there is enough data to make a plot and then make the SC vs CL plot
  
  if(nrow(Plotdata)>1){
    
    if(!allData){ png(file=path, width = 700, height = 600) } 
  par(oma = c(4, 1, 1, 1)) # this makes a bigger area so that the legend can go below the plot  
  par(mar = c(6,4,3,2)+0.1) # Default margin is (5,4,4,2)+0.1 this decreases the margin on the top and bottom so that the 
  # the plot is a little taller which provides room for the longer tic lables with the comma.
  plot(Plotdata$`Specific conductance`, Plotdata$`Chloride concentration`, 
       type = "p", col="blue", main=plotTitle, 
       xlab = Sctitle, 
       ylab = Cltitle, 
       yaxt='n',
       xaxt='n') # yaxt='n' and xaxt='n' set the labels to not display so that you can add your own.
  axis(1, at=axTicks(1), labels=format(axTicks(1), big.mark=','), las=1, tck=0.05) # this adds commas for thousands to the axis labels.
  axis(2, at=axTicks(2), labels=format(axTicks(2), big.mark=','), las=0, tck=0.05) # this adds commas for thousands to the axis labels.las=0 makes the labels parrelel to the axis.
  axis(3, labels=FALSE, tck=0.05)
  axis(4, labels=FALSE, tck=0.05)
  mtext("U.S. Geological Survey", adj=0, col="dark green")
  mtext("Graph includes provisional data", side=3, adj=1,col="magenta", cex=0.8)
  
  # compute a linear regression line and then add it to the plot
  
  Rline<-lm(Plotdata$`Chloride concentration`~ Plotdata$`Specific conductance`)
  if(!is.na(Rline$coefficients[2])){abline(Rline)}
  
  
  # Determine the slope intercept and R-squared of the linear regression then paste this info into text variables
  SumRline<-summary(Rline)
  ClvsSC_rsqr<-SumRline$r.squared
  CLvsSC_intcp<-Rline$coefficients[1]
  CLvsSC_slope<-Rline$coefficients[2]
  lmresults<-as.data.frame(cbind(round(CLvsSC_intcp,1), round(CLvsSC_slope,4), round(ClvsSC_rsqr,4)))
  names(lmresults)<-c("Intercept", "Slope", "R-squared")
  Ftext1<-paste0("  y=",lmresults$Slope,"x",lmresults$Intercept)
  Ftext2<-paste0("  r-squared=", lmresults$`R-squared`)
 
  
  # Put this information and a ledgend on the plot
 
 
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0,0,0,0), new = TRUE) # this sets the legend up
  legend("bottomright", 
         c("EXPLANATION", "Linear regression", Ftext1, Ftext2), 
         lty=c(0,1,0,0), 
         col=c(NA,"black",NA,NA), 
         xpd = TRUE, 
         horiz = FALSE, 
         inset = c(0, -0.22))
  
  # xpd = TRUE tells R that it is OK to plot outside the region 
  # horiz = TRUE tells R that I want a horizontal legend 
  # inset = c(x,y) tells R how to move the legend relative to the 'bottom' location
  
  
  # Record and close out the plot
  
  ClvsScPlot <- recordPlot()
  dev.off()
   }
  
  
  # Make a table of the specific conductance versus chloride data plotted
  
  
  
  ScCltable <- Plotdata %>% 
    select(Date, `Station ID`, `Specific conductance`, `Chloride concentration`) %>% 
    setNames(c('Date', 'Station ID', Sctitle, Cltitle))
  
  
  
  ScCltable2 <- Plotdata %>% 
    select(Date, `Specific conductance`, `Chloride concentration`) %>% 
    setNames(c('Date', Sctitle, Cltitle))
  
  
  ScCltable2[,2]<-prettyNum(ScCltable2[,2], big.mark = ",", scientific = FALSE)
  ScCltable2[,3]<-prettyNum(ScCltable2[,3], big.mark = ",", scientific = FALSE)
  
  
  
  ScCltable_xtable2 <- ScCltable2 %>% mutate(Date = as.character(Date)) #xtable throws warning for Date class columns
  
  
 
  # print ScCltable2  to a file
  
    print(xtable(ScCltable_xtable2), type="html", file=Tablepath, include.rownames=FALSE)
  
  
  
  return(list(table = ScCltable))
}

#___________________________________________________________________________________
#
#***********************   END ScPlottable    **************************
#
#___________________________________________________________________________________





#___________________________________________________________________________________
#
#***********************   FUNCTION chlorideTimeSeriesTable    *********************
#
#___________________________________________________________________________________
#
# Not the best name for this funtion - This function creates all the time series tables and plots
# not just the chloride time series tables.
#
chlorideTimeseriesTable <- function(MainDir, FullSiteName, dat, trendResults_i,trendResults_i2, pc){
  
  # test1<-tryCatch(is.null(trendResults_i), error=function(cond) TRUE)
  
  # if(test1 == FALSE) {print(c("in chorideTimeSeriesTable", trendResults_i,trendResults_i2))}
  
  # Create a path including plot name for each plot
  path <- paste(MainDir, "plots/", FullSiteName, "_", pc, ".png", sep='')
  TabPath <- paste(MainDir,"tables/",FullSiteName,"_",pc,".html",sep='')
  ylabel <- getParmCodeDef(pc)
  
#********** Create plots of the data by station ID and parameter code.
 
#******************** MAKE PLOTS and STATISCAL TABLES FOR CHLORIDEs *******************
  
  if(pc == "99220"){
    
  # Set up and make chloride plots for each site 
    
    png(file=path, width = 700, height = 600)
    par(oma = c(4, 1, 1, 1)) # this makes a bigger area so that the legend can go below the plot
    par(mar = c(6,4,3,2)+0.1) # Default margin is (5,4,4,2)+0.1 this decreases the margin on the top and bottom so that the 
    # the plot is a little taller which provides room for the longer tic lables with the comma.
    plot(dat$sample_dt,
         dat$result_va, 
         type = "p", 
         col="white", 
         main=FullSiteName, 
         xlab = "Date", 
         ylab = ylabel,
         tck=0.05,
        yaxt='n') # yaxt='n' and xaxt='n' set the tic labels to not display so that you can add your own.
    #axis(1, labels=FALSE, tck=0.05)
    axis(2, tck=0.05, at=axTicks(2), labels=format(axTicks(2), big.mark=','), las=0) # this adds commas for thousands to the axis labels.las=0 makes the labels parrelel to the axis.
    axis(3, labels=FALSE, tck=0.05)
    axis(4, labels=FALSE, tck=0.05)
    mtext("U.S. Geological Survey", adj=0, col="dark green")
    mtext("Graph includes provisional data", side=3, adj=1,col="magenta", cex=0.8)
 
    # Color code the points in the chloride plot by chloride result
    
    
    RedPoints<-dat%>%
      filter(result_va >= 1000)
    OrangePoints <- dat%>%
      filter(result_va >= 250 & result_va < 1000)
    BluePoints <-dat%>%
      filter(result_va < 250)
    points(RedPoints$sample_dt,RedPoints$result_va, pch=0, col="red")
    points(OrangePoints$sample_dt,OrangePoints$result_va, pch=5, col="orange")
    points(BluePoints$sample_dt,BluePoints$result_va, pch=2, col="blue") 
    
    if(nrow(trendResults_i) > 0){
   
    # add the trend lines to the plots as line segments 
    
    for(i in 1:length(trendResults_i$trendType)){
     
      if(trendResults_i$trendType[i] == "Five Year"){
        startDate<-as.numeric(trendResults_i$lastSampleDate[i])-5*365.25
        endDate<-as.numeric(trendResults_i$lastSampleDate[i])
        firstValue<-startDate*trendResults_i$slope[i]+trendResults_i$intercept[i]
        endValue<-endDate*trendResults_i$slope[i]+trendResults_i$intercept[i]
        segments(x0 = startDate, x1= endDate, y0 = firstValue, y1 = endValue, col="green", lty=5)
      } else if (trendResults_i$trendType[i] == "Twenty Year"){
        startDate<-as.numeric(trendResults_i$lastSampleDate[i])-20*365.25
        endDate<-as.numeric(trendResults_i$lastSampleDate[i])
        firstValue<-startDate*trendResults_i$slope[i]+trendResults_i$intercept[i]
        endValue<-(endDate*trendResults_i$slope[i]+trendResults_i$intercept[i])
        segments(x0 = startDate, x1= endDate, y0 = firstValue, y1 = endValue, col="dark green", lty=4)
        
      }
    }
      
    }
    # add the ledgend, USGS Identifier, and provisional data statement to the plot
    
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE) # this sets the legend up
    legend("bottomright", 
           c("EXPLANATION", "Choride concentration,", "in milligrams per liter", "   <250", "   >=250 and <1,000","   >=1,000", "Trend", "   5 year", "   20 year"), 
           lty=c(NA,NA,NA,NA,NA,NA,NA,5,4), 
           pch=c(NA,NA, NA,2,5,0, NA, NA, NA),
           col=c(NA,NA, NA, "blue","orange", "red", NA, "green", "dark green"),
           merge=FALSE,
           xpd = TRUE, 
           horiz = FALSE, 
           inset = c(0, -0.22)
           )
    
    
  #**********************  CREATE TABLE OF STATS AND CHLORIDE INFO ********************************
  #
    
  FirstCLof250 <- NA
  FirstCLof1000 <- NA
  if(length(OrangePoints$sample_dt)>0){FirstCLof250<-OrangePoints$sample_dt[1]}
  if(length(RedPoints$sample_dt)>0){FirstCLof1000<-RedPoints$sample_dt[1]}
 
  CLSumry <- data.frame(as.list(summary(dat$result_va)))
  
  
    
    CLSumry <- rbind(
      c("Date of first sample", as.character(dat$sample_dt[1])),
      c("First sample result (mg/L)", prettyNum(dat$result_va[1],big.mark = ",", scientific = FALSE)),
      c("Date of last sample", as.character(dat$sample_dt[nrow(dat)])),
      c("Last sample result (mg/L)", prettyNum(dat$result_va[nrow(dat)],big.mark = ",", scientific = FALSE)),
      c("Date of first sample within 250 to 999 mg/L", as.character(FirstCLof250)),
      c("Date of first sample with 1,000 mg/L or greater",as.character(FirstCLof1000)),
      c("Minimum (mg/L)", prettyNum(CLSumry$Min.,big.mark = ",", scientific = FALSE)),
      c("Maximum (mg/L)", prettyNum(CLSumry$Max.,big.mark = ",", scientific = FALSE)),
      c("Mean (mg/L)", prettyNum(CLSumry$Mean,big.mark = ",", scientific = FALSE)),
      c("First quartile (mg/L)", prettyNum(CLSumry$X1st.Qu.,big.mark = ",", scientific = FALSE)),
      c("Median (mg/L)", prettyNum(CLSumry$Median,big.mark = ",", scientific = FALSE)),
      c("Third quartile (mg/L)", prettyNum(CLSumry$X3rd.Qu.,big.mark = ",", scientific = FALSE)),
      c("Number of samples", nrow(dat))
      
    )
  CLSumry<-as.data.frame(CLSumry)
  names(CLSumry)<-c("Analysis", "Result")
  CLSumTabPath <- paste(MainDir,"tables/",FullSiteName,"_",pc,"sum",".html",sep='')
  print(xtable(CLSumry), type="html", file=CLSumTabPath, include.rownames=FALSE)
  
  #**********************   END OF CREATING CHLORIDE STATS TABLE  **************
  #
  # *****************  cREATE A TABLE WITH THE TREND LINE SLOPE AND INTERCEPT *********************
  #
  
  CLtrendTab<-trendResults_i2%>% 
    select(trendType, trend, pValue, tau, slope, intercept)%>%
    filter(trendType != 'Overall')
  names(CLtrendTab)<- c("Trend test", "Test result", "P-value", "Kendall's tau","Slope", "Intercept")
  # The following block of code rounds off the intercept.
  for (i in 1:length(CLtrendTab$Intercept)){
      # print(CLtrendTab$Intercept[i])
    if(!is.na(CLtrendTab$Intercept[i])){
      if (CLtrendTab$Intercept[i] > 1 | CLtrendTab$Intercept[i] < -1 ){CLtrendTab$Intercept[i]<-round(CLtrendTab$Intercept[i], digits =  0)} else{CLtrendTab$Intercept[i]<-round(CLtrendTab$Intercept[i], digits = 2)}
      
      }
  }
  # This adds commas for thousands
  CLtrendTab$Intercept<-prettyNum(CLtrendTab$Intercept, big.mark = ",", scientific = FALSE)
  CLTrendTabPath <- paste(MainDir,"tables/",FullSiteName,"_",pc,"trend",".html",sep='')
  print(xtable(CLtrendTab), type="html", file=CLTrendTabPath, include.rownames=FALSE)
  
  #
  # ************************ END OF cREATING TABLE WITH  TREND LINE SLOPE AND INTERCEPT ***********
  
  dev.off()
  
  }
  
  #
  # ***********************END OF CREATING CHLORIDE PLOT  ****************
  #
  
  
  # *********************** CREATE TABLES THAT HAVE THE DATA FOR EACH PLOT ************************ 
 # ylabel<-"ylabel"
  dat2 <- dat %>% 
    select(sample_dt, result_va) %>% 
    setNames(c('Date', ylabel))
  
  # Convert dat2 into a data frame so that pretty num will work
 # dat2 <-as.data.frame.matrix(dat2)
 
  # Pull the values from column 2 from dat2. By default its pulled as a list so you have to unlist it. 
  dat2data<-unlist(dat2[,2], use.names = FALSE)
  # put commas into data2data for thousands, and then stick these number back into the original data frame.
  dat2[,2]<-prettyNum(dat2data, big.mark = ",", scientific = FALSE)
  
  cbind(dat2[,1],dat2data)
  
  dat2_xtable <- dat2 %>% mutate(Date = as.character(Date)) #xtable throws warning for Date class columns
  print(xtable(dat2_xtable), type="html", file=TabPath, include.rownames=FALSE)
  
  
  # ************************ END OF CREATING TABLES WITH PLOT DATA ********************************
  
  # **************** CREATE A sPECIFIC cONDUCTANCE PLOT AND SUMMARY STATS TABLE *******************
  
  if(pc == "90095" && nrow(dat) > 0){
    
    # Open the plot device and make the plot
    
    png(file=path, width = 700, height = 600)
    par(oma = c(4, 1, 1, 1)) # this makes a bigger area so that the legend can go below the plot
    par(mar = c(6,4,3,2)+0.1) # Default margin is (5,4,4,2)+0.1 this decreases the margin on the top and bottom so that the 
    # the plot is a little taller which provides room for the longer tic lables with the comma.
    
    plot(dat$sample_dt,
         dat$result_va, 
         type = "p", 
         col="blue", 
         main=FullSiteName, 
         xlab = "Date", 
         ylab = ylabel, 
         tck=0.05,
         yaxt='n') # yaxt='n' and xaxt='n' set the tic labels to not display so that you can add your own.
    axis(2, tck=0.05, at=axTicks(2), labels=format(axTicks(2), big.mark=','), las=0) # this adds commas for thousands to the axis labels.las=0 makes the labels parrelel to the axis.
    axis(3, labels=FALSE, tck=0.025)
    axis(4, labels=FALSE, tck=0.025)
    
    # add USGS ID and provisional data statement
    
    mtext("U.S. Geological Survey", adj=0, col="dark green")
    mtext("Graph includes provisional data", side=3, adj=1,col="magenta", cex=0.8)
    
    # Compute the specific conductance summary statistics and create a table
    
  SCSumry <- data.frame(as.list(summary(dat$result_va)))
  units_str <- paste0("(", "&mu;","S/cm)")
  SCSumry <- rbind(
    c("Date of first sample", as.character(dat$sample_dt[1])),
    c(paste("First sample result", units_str), prettyNum(dat$result_va[1],big.mark = ",", scientific = FALSE)),
    c("Date of last sample",as.character(dat$sample_dt[nrow(dat)]) ),
    c(paste("Last sample result", units_str), prettyNum(dat$result_va[nrow(dat)],big.mark = ",", scientific = FALSE)),
    c(paste("Minimum", units_str), prettyNum(SCSumry$Min.,big.mark = ",", scientific = FALSE)),
    c(paste("Maximum", units_str), prettyNum(SCSumry$Max.,big.mark = ",", scientific = FALSE)),
    c(paste("Mean", units_str), prettyNum(SCSumry$Mean,big.mark = ",", scientific = FALSE)),
    c(paste("First quartile", units_str), prettyNum(SCSumry$X1st.Qu.,big.mark = ",", scientific = FALSE)),
    c(paste("Median", units_str), prettyNum(SCSumry$Median,big.mark = ",", scientific = FALSE)),
    c(paste("Third quartile", units_str), prettyNum(SCSumry$X3rd.Qu.,big.mark = ",", scientific = FALSE)),
    c("Number of samples", nrow(dat))
    
  )
  # The character string "&mu;" puts the Greek character mu in the text string - see below
  # The prettyNum function adds a comma to separate thousands
  SCSumry<-as.data.frame(SCSumry)
  names(SCSumry)<-c("Analysis", "Result")
  SCSumTabPath <- paste(MainDir,"tables/",FullSiteName,"_",pc,"sum",".html",sep='')
  
  # Print specific conductance  summary stats table to a file
  
  #print(xtable(SCSumry), type="html", file=SCSumTabPath, include.rownames=FALSE)
  
  library(xtable)
  print(xtable(SCSumry), type='html', file=SCSumTabPath, 
        include.rownames=FALSE, sanitize.text.function = function(x) x) # the sanitize function converst the &mu: term into the greek character mu. 
  
  library(htmlTable)
  # print(htmlTable(SCSumry, rnames=F), file=SCSumTabPath)
  
  
  
  # close out the plot device
  
  dev.off()
  }
  
  # **************** CREATE A PLOT FOR WATER LEVELS *******************
  
  if(pc == "72020" && nrow(dat) > 0){
    png(file=path, width = 750, height = 450)
    plot(dat$sample_dt,dat$result_va, type = "p", col="blue", main=FullSiteName, 
         xlab = "Date", ylab = ylabel, tck=0.05)
    axis(3, labels=FALSE, tck=0.025)
    axis(4, labels=FALSE, tck=0.025)
    mtext("U.S. Geological Survey", adj=0, col="dark green")
    mtext("Graph includes provisional data", side=3, adj=1,col="magenta", cex=0.8)
    dev.off()
  }
  
  # DONE
  
  return()
}

#___________________________________________________________________________________
#
#***********************   END FUNCTION chlorideTimeSeriesTable    *****************
#
#___________________________________________________________________________________