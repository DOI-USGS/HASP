

# ********************************************************************************************************
# R SCRIPT                                                                                               *
# Install Packages                                                                                       *
#                                                                                                        *
# This script is used as set up for all my other scripts. It installs the packages I routine need        *
# Created by Scott Prinos, Hydrologist, USGS Carebean-Florida Water Science Center, Davie Florida        *
# June 1, 2018                                                                                           *
#                                                                                                        *
#*********************************************************************************************************
#
#
# The following information is for set up only and does not need to be rerun each time
# 
# 
#
options(repos = c(CRAN = "https://cloud.r-project.org/"))

.libPaths(c(.libPaths(), "C:/Users/stprinos/Documents/R/win-library/3.3"))

install.packages("openxlsx")
install.packages("dataRetrieval")
install.packages("xtable")
install.packages("xlsx")
install.packages("dplyr")
install.packages("EnvStats")
install.packages("lubridate")
install.packages("Kendall")
install.packages("htmlTable")
install.packages("stringr", repos='http://cran.us.r-project.org')

