## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
opts_chunk$set(echo = TRUE,
               warning = FALSE,
               message = FALSE,
               fig.width = 6,
               fig.height = 6)

options(knitr.kable.NA = '')
library(dplyr)

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
#  # Water quality data:
#  pcodes_cl_sc <- c("00095","90095","00940","99220")
#  qw_data <- readNWISqw(siteID,  pcodes_cl_sc)
#  

## ----getDataReal--------------------------------------------------------------
siteID <- "263819081585801"
parameterCd <- "62610"
statCd <- "00001"

# Daily
gw_level_dv <-  L2701_example_data$Daily
# Periodic
gwl_data <- L2701_example_data$Discrete
# QW:
qw_data <- L2701_example_data$QW

plot_title <- "L -2701\n263819081585801 "


## ----monthlyFrequencyPlot, eval=FALSE-----------------------------------------
#  
#  monthly_frequency_plot(gwl_data,
#                         plot_title = plot_title,
#                         plot_range = "Past year")
#  
#  monthly_frequency_table(gwl_data)

## ----monthlyFreqPlot, echo = FALSE--------------------------------------------
monthly_frequency_plot(gwl_data, 
                       plot_title = plot_title,
                       plot_range = "Past year")

## ----monthlyFreqTable, echo = FALSE-------------------------------------------
monthly_freq_table <- monthly_frequency_table(gwl_data) %>%
  select(month, minMed, p10, p25, p50, p75, p90, maxMed, nYears) %>%
  mutate(month = month.abb[month]) %>%
  rename("Month" = month,
         "Lowest median" = minMed,
         "10th" = p10,
         "25th" = p25,
         "50th" = p50,
         "75th" = p75,
         "Highest median" = maxMed,
         "Number of years" = nYears)
kable(monthly_freq_table, digits = 1)

## ----dailyPlot, eval = FALSE--------------------------------------------------
#  
#  daily_gwl_2yr_plot(gw_level_dv,
#                     parameterCd,
#                     statCd,
#                     plot_title = plot_title,
#                     month_breaks = TRUE,
#                     historical_stat = "median")
#  
#  daily_gwl_summary(gw_level_dv, parameterCd, statCd)

## ----dailyPlotShow, echo = FALSE, fig.width=9---------------------------------

daily_gwl_2yr_plot(gw_level_dv, 
                   parameterCd, 
                   statCd, 
                   plot_title = plot_title, 
                   month_breaks = TRUE, 
                   historical_stat = "median")

## ----dailySummary, echo=FALSE-------------------------------------------------
daily_summary_table <- daily_gwl_summary(gw_level_dv, parameterCd, statCd) %>%
  select("Begin Date" = begin_date,
         "End Date" = end_date,
         "Days" = days,
         "% Complete" = percent_complete,
         "Lowest Level" = lowest_level,
         "25th" = p25,
         "50th" = p50,
         "75th" = p75,
         "Highest Level" = highest_level)
kable(daily_summary_table)

## ----periodicPlotDisplay, eval=FALSE------------------------------------------
#  gwl_plot_periodic(gwl_data, plot_title)
#  
#  site_data_summary(gwl_data, "sl_lev_va")

## ----periodicPlot, echo=FALSE-------------------------------------------------
gwl_plot_periodic(gwl_data, plot_title)

## ----periodicTable, echo=FALSE------------------------------------------------
sitePeriodic <- site_data_summary(gwl_data, 
                                  "sl_lev_va") %>% 
  select(Minimum = min_site, 	
         `1st` = p25, 
         Median = p50,
         Mean = mean_site,
         `3rd` = p75,
         Maximum = max_site)

kable(sitePeriodic, digits = 1)


## ----dvPlotDisplay, eval=FALSE------------------------------------------------
#  gwl_plot_all(gw_level_dv, gwl_data, plot_title)
#  
#  site_data_summary(gw_level_dv, "X_62610_00001")

## ----dvPlot, echo=FALSE-------------------------------------------------------
gwl_plot_all(gw_level_dv, gwl_data, plot_title)

## ----dailyTable, echo=FALSE---------------------------------------------------
siteDaily <- site_data_summary(gw_level_dv, 
                               "X_62610_00001") %>% 
  select(Minimum = min_site, 	
         `1st` = p25, 
         Median = p50,
         Mean = mean_site,
         `3rd` = p75,
         Maximum = max_site)

kable(siteDaily, digits = 1)


## ----chloride_plot, eval=FALSE------------------------------------------------
#  trend_plot(qw_data,
#             plot_title = plot_title,
#             pcode = c("00940","99220"))
#  
#  kendell_test_5_20_years(dplyr::filter(qw_data,
#                                        parm_cd %in% c("00940","99220")),
#                          seasonal = TRUE,
#                          enough_5 = 1,
#                          enough_20 = 1,
#                          date_col = "sample_dt",
#                          value_col = "result_va")
#  
#  qw_summary(qw_data,
#             pcode = c("00940","99220"),
#             norm_range = c(225,999))

## ----chloride_plotSHOW, echo=FALSE--------------------------------------------
trend_plot(qw_data, plot_title = plot_title)

## ----chloride_trend, echo=FALSE-----------------------------------------------
cl_trend <- kendell_test_5_20_years(dplyr::filter(qw_data, 
                                      parm_cd %in% c("00940","99220")), 
                     seasonal = TRUE,
                     enough_5 = 1,
                     enough_20 = 1,
                     date_col = "sample_dt",
                     value_col = "result_va")
kable(cl_trend, digits = 3)

## ----chloride_table, echo=FALSE-----------------------------------------------
chl_table <- qw_summary(qw_data, 
                       pcode = c("00940","99220"),
                       norm_range = c(225,999))
kable(chl_table)


## ----qwplot1, eval=FALSE------------------------------------------------------
#  qw_plot(qw_data, "Specific Conductance",
#          pcode = c("00095", "90095"))
#  qw_summary(qw_data,
#             pcode = c("00095","90095"),
#             norm_range = NA)

## ----qwplotReal, echo=FALSE---------------------------------------------------
qw_plot(qw_data, "Specific Conductance", 
        pcode = c("00095", "90095"))

## ----sc_table, echo=FALSE-----------------------------------------------------
sc_table <- qw_summary(qw_data, 
                       pcode = c("00095","90095"),
                       norm_range = NA)
kable(sc_table)


## ----salityPlot, eval=FALSE---------------------------------------------------
#  Sc_Cl_plot(qw_data, plot_title = plot_title)
#  
#  Sc_Cl_table(qw_data)

## ----salityPlotDisplay, echo=FALSE--------------------------------------------
Sc_Cl_plot(qw_data, plot_title = plot_title)


## ----salityTable, echo=FALSE--------------------------------------------------
sc_cl <- Sc_Cl_table(qw_data)

# only show 10 row:
kable(head(sc_cl, 10), 
      col.names = c("Date",
                    "Chloride [mg/L]",
                    "Specific conductance [µS/L]"))

