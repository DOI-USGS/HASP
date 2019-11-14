#' getParmCodeDef
#' 
#' Function provides the text description for the parameter codes
#' 
#' @param pc parameter code
#' @export
#' @examples 
#' pc <- "99220"
#' getParmCodeDef(pc)
#' #TODO: replace this function with readNWISpcode!
getParmCodeDef <- function(pc){
  
  switch(pc, 
         "99220" = "Chloride concentration, in milligrams per liter",
         "90095" = "Specific conductance, in microsiemens per liter",
         "72020" = "Water level elevation, in feet"
         )
}



#' ScCLPlotTable 
#'
#' Function creates the individual chloride versus specfic conductance plots for each site
#' as well as the chloride versus specific conductance plot for all sites.
#'
#' @param Plotdata data frame
#' @param plotTitle character
#' @param allData logical
#' @export
#' @import dplyr
#'
ScClPlotTable <- function(Plotdata, plotTitle, allData = FALSE){
  
  # Specify the plot titles using the function getParmCodeDef
  
  Cltitle <- getParmCodeDef("99220")
  Sctitle <- getParmCodeDef("90095")
  
  #different column names depending on whether it is all the data (ClvsSC_all) or not
  Plotdata <- Plotdata %>% 
    select(Date, `Station ID`, 
           `Specific conductance` = matches("Specific conductance"), 
           `Chloride concentration` = matches("Chloride concentration"))

  # Create SC versus chloride plots of the data by station ID and parameter code.
  
  # Make sure there is enough data to make a plot and then make the SC vs CL plot
  
  if(nrow(Plotdata)>1){
    
    par(oma = c(4, 1, 1, 1)) # this makes a bigger area so that the legend can go below the plot  
    par(mar = c(6,4,3,2)+0.1) # Default margin is (5,4,4,2)+0.1 this decreases the margin on the top and bottom so that the 
    # the plot is a little taller which provides room for the longer tic lables with the comma.
    plot(Plotdata$`Specific conductance`, 
         Plotdata$`Chloride concentration`, 
         type = "p", col="blue", 
         main=plotTitle, 
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
    
    Rline <- lm(Plotdata$`Chloride concentration`~ Plotdata$`Specific conductance`)
    
    if(!is.na(Rline$coefficients[2])){abline(Rline)}
    
    # Determine the slope intercept and R-squared of the linear regression then paste this info into text variables
    SumRline <- summary(Rline)
    ClvsSC_rsqr <- SumRline$r.squared
    CLvsSC_intcp <- Rline$coefficients[1]
    CLvsSC_slope <- Rline$coefficients[2]
    lmresults <- as.data.frame(cbind(round(CLvsSC_intcp,1), round(CLvsSC_slope,4), round(ClvsSC_rsqr,4)))
    names(lmresults) <- c("Intercept", "Slope", "R-squared")
    Ftext1<-paste0("  y=", lmresults$Slope, "x", lmresults$Intercept)
    Ftext2<-paste0("  r-squared=", lmresults$`R-squared`)
    
    # Put this information and a ledgend on the plot
    
    par(fig = c(0, 1, 0, 1), 
        oma = c(0, 0, 0, 0), 
        mar = c(0,0,0,0), 
        new = TRUE) # this sets the legend up
    legend("bottomright", 
           c("EXPLANATION", "Linear regression", Ftext1, Ftext2), 
           lty = c(0, 1, 0, 0), 
           col = c(NA, "black", NA, NA), 
           xpd = TRUE, 
           horiz = FALSE, 
           inset = c(0, -0.22))
    
    # xpd = TRUE tells R that it is OK to plot outside the region 
    # horiz = TRUE tells R that I want a horizontal legend 
    # inset = c(x,y) tells R how to move the legend relative to the 'bottom' location
    
  }
  
  # Make a table of the specific conductance versus chloride data plotted
  
  ScCltable <- Plotdata %>% 
    select(Date, `Station ID`, `Specific conductance`, `Chloride concentration`) %>% 
    setNames(c('Date', 'Station ID', Sctitle, Cltitle))
  
  ScCltable2 <- Plotdata %>% 
    select(Date, `Specific conductance`, `Chloride concentration`) %>% 
    setNames(c('Date', Sctitle, Cltitle))
  
  ScCltable2[,2] <- prettyNum(ScCltable2[,2], big.mark = ",", scientific = FALSE)
  ScCltable2[,3] <- prettyNum(ScCltable2[,3], big.mark = ",", scientific = FALSE)
  
  ScCltable_xtable2 <- ScCltable2 %>%
    mutate(Date = as.character(Date)) 

  return(list(table = ScCltable))
}

