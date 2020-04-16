## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
opts_chunk$set(echo = TRUE,
               warning = FALSE,
               message = FALSE,
               fig.width = 6,
               fig.height = 6)
library(dplyr)
library(tidyr)
library(ggplot2)
library(HASP)

options(knitr.kable.NA = '')

## ----siteInfo, results='asis'-------------------------------------------------
library(HASP)

siteID <- "263819081585801"
site_metadata <- site_summary(siteID, markdown = TRUE)

## ----whatData-----------------------------------------------------------------
data_info <- data_available(siteID)
kable(data_info)

## ----getData, eval=FALSE------------------------------------------------------
#  library(dataRetrieval)
#  siteID <- "263819081585801"
#  parameterCd <- "62610"
#  statCd <- "00001"
#  # Daily data:
#  gw_level_dv <- readNWISdv(siteID,
#                            parameterCd,
#                            statCd = statCd)
#  
#  # Periodically measured:
#  gwl_data <- readNWISgwl(siteID)
#  

## ----getDataReal--------------------------------------------------------------
siteID <- "263819081585801"
parameterCd <- "62610"
statCd <- "00001"

# Daily
gw_level_dv <-  L2701_example_data$Daily
# Periodic
gwl_data <- L2701_example_data$Discrete

site_title <- "L -2701\n263819081585801"


## ----weeklyFrequencyShow, eval=FALSE------------------------------------------
#  
#  weekly_frequency_plot(gw_level_dv,
#                        parameterCd, statCd,
#                        plot_title = site_title)
#  
#  weekly_table <- weekly_frequency_table(gw_level_dv,
#                                         parameterCd,
#                                         statCd)
#  
#  head(weekly_table, 10)

## ----weeklyFrequencyPlot, echo=FALSE, fig.width=9-----------------------------
weekly_frequency_plot(gw_level_dv, 
                      parameterCd, statCd, 
                      plot_title = site_title)


## ----weekly_table, echo=FALSE-------------------------------------------------
library(dplyr)

weekly_freq_table <- weekly_frequency_table(gw_level_dv,
                                            parameterCd,
                                            statCd) %>%
  select("Week" = week,
         "Lowest median" = minMed,
         "10th" = p10,
         "25th" = p25,
         "50th" = p50,
         "75th" = p75,
         "90th" = p90,
         "Highest median" = maxMed,
         "# Years" = nYears)
kable(head(weekly_freq_table, 10), digits = 1)


## ----dailyPlotShow, eval=FALSE------------------------------------------------
#  daily_gwl_2yr_plot(gw_level_dv,
#                     p_code_dv = parameterCd,
#                     statCd = statCd,
#                     plot_title = site_title,
#                     historical_stat = "mean",
#                     month_breaks = TRUE)
#  
#  daily_table <- daily_frequency_table(gw_level_dv,
#                        p_code_dv = parameterCd,
#                        statCd = statCd)
#  head(daily_table, n = 10)

## ----dailyDo, echo=FALSE, fig.width=9-----------------------------------------
daily_gwl_2yr_plot(gw_level_dv, 
                   p_code_dv = parameterCd, 
                   statCd = statCd,
                   plot_title = site_title,
                   historical_stat = "mean",
                   month_breaks = TRUE)


## ----dailyShow2, echo=FALSE---------------------------------------------------
daily_table <- daily_frequency_table(gw_level_dv, 
                                     p_code_dv = parameterCd,
                                     statCd = statCd) %>%
  rename("DOY" = DOY,
         "Maximum" = max,
         "Mean" = mean,
         "Minimum" = min,
         "# Points" = points)
kable(head(daily_table, 10))

## ----trend_dv, eval=FALSE-----------------------------------------------------
#  
#  gwl_plot_all(gw_level_dv, plot_title = site_title,
#               gwl_data, add_trend = TRUE,
#               p_code_dv = parameterCd)
#  
#  site_data_summary(gw_level_dv, "X_62610_00001")
#  
#  gw_monthly <- monthly_mean(gw_level_dv)
#  
#  kendell_test_5_20_years(gw_monthly,
#                          seasonal = TRUE,
#                          date_col = "mid_date",
#                          value_col = "mean_va")

## ----gwlTrendplot, echo=FALSE-------------------------------------------------
gwl_plot_all(gw_level_dv, plot_title = site_title,
             gwl_data, add_trend = TRUE,
             p_code_dv = parameterCd)



## ----siteStatsDaily, echo=FALSE-----------------------------------------------
siteDV <- site_data_summary(gw_level_dv, "X_62610_00001") %>% 
  select(Minimum = min_site, 	
         `1st` = p25, 
         Median = p50,
         Mean = mean_site,
         `3rd` = p75,
         Maximum = max_site)

kable(siteDV, digits = 1)


## ----trendTableDaily, echo=FALSE----------------------------------------------
gw_monthly <- monthly_mean(gw_level_dv)

trend_result <- kendell_test_5_20_years(gw_monthly, 
                        seasonal = TRUE, 
                        date_col = "mid_date", 
                        value_col = "mean_va")
kable(trend_result, digits = 1)



## ----manual, eval=FALSE-------------------------------------------------------
#  gwl_plot_all(NULL, gwl_data, plot_title = site_title)
#  
#  site_data_summary(gwl_data, "sl_lev_va")

## ----manualPlot, echo=FALSE---------------------------------------------------
gwl_plot_all(NULL, gwl_data, plot_title = site_title)



## ----manualTable, echo=FALSE--------------------------------------------------
quantiles <- site_data_summary(gwl_data, "sl_lev_va") %>% 
  select(Minimum = min_site, 	
         `1st` = p25, 
         Median = p50,
         Mean = mean_site,
         `3rd` = p75,
         Maximum = max_site)
kable(quantiles, digits = 1)


