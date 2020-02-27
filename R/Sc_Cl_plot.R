#' ScCLPlotTable 
#'
#' Function creates the individual chloride versus specfic conductance plots for each site
#' as well as the chloride versus specific conductance plot for all sites.
#'
#' @param x data frame
#' @param title character
#' @export
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @import graphics
#' @importFrom dataRetrieval readNWISpCode
#' @importFrom stats lm
#' @examples 
#' 
#' site <- '254457080160301'
#' site_data <- dataRetrieval::readNWISqw(site, 
#'                                        parameterCd = c("99220","90095"))
#' Sc_Cl_plot(site_data, title = "Hi")
Sc_Cl_plot <- function(x, title){
  
  # Specify the plot titles using the function getParmCodeDef
  
  sample_dt <- startDateTime <- site_no <- parm_cd <- remark_cd <- result_va <- `90095` <- `99220` <- ".dplyr"
  
  Cltitle <- readNWISpCode("99220")[["parameter_nm"]]
  Sctitle <- readNWISpCode("90095")[["parameter_nm"]]
  
  #different column names depending on whether it is all the data (ClvsSC_all) or not
  Plotdata <- x %>% 
    select(Date = sample_dt, 
           startDateTime,
           `Station ID` = site_no, 
           parm_cd, remark_cd, result_va) %>% 
    pivot_wider(names_from = parm_cd, 
                values_from = result_va) %>%  
    rename(cloride = `99220`,
           sp = `90095`)
  
  # Create SC versus chloride plots of the data by station ID and parameter code.
  
  # Make sure there is enough data to make a plot and then make the SC vs CL plot
  
  if(nrow(Plotdata)>1){
    
    par(oma = c(4, 1, 1, 1)) # this makes a bigger area so that the legend can go below the plot  
    par(mar = c(6,4,3,2)+0.1) # Default margin is (5,4,4,2)+0.1 this decreases the margin on the top and bottom so that the 
    # the plot is a little taller which provides room for the longer tic lables with the comma.
    plot(Plotdata$sp, 
         Plotdata$cloride, 
         type = "p", col="blue", 
         main = title, 
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
    
    Rline <- lm(Plotdata$cloride ~ Plotdata$sp)
    
    if(!is.na(Rline$coefficients[2])){
      abline(Rline)
    }
    
    # Determine the slope intercept and R-squared of the linear regression then paste this info into text variables
    SumRline <- summary(Rline)
    ClvsSC_rsqr <- SumRline$r.squared
    CLvsSC_intcp <- Rline$coefficients[1]
    CLvsSC_slope <- Rline$coefficients[2]
    lmresults <- as.data.frame(cbind(round(CLvsSC_intcp,1), round(CLvsSC_slope,4), round(ClvsSC_rsqr,4)))
    names(lmresults) <- c("Intercept", "Slope", "R-squared")
    Ftext1 <- paste0("  y=", lmresults$Slope, "x", lmresults$Intercept)
    Ftext2 <- paste0("  r-squared=", lmresults$`R-squared`)
    
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
    
  }
  
}
