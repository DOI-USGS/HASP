## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
opts_chunk$set(echo = FALSE,
               warning = FALSE,
               message = FALSE,
               fig.width = 7,
               fig.height = 7)
library(dplyr)
library(tidyr)
library(ggplot2)
library(HASP)

options(knitr.kable.NA = '')

## ----siteInfo, echo=TRUE, results='asis'--------------------------------------
library(dataRetrieval)
library(HASP)
library(dplyr)
library(tidyr)

siteID <- "263819081585801"
site_metadata <- site_summary(siteID, markdown = TRUE)

## ----whatData, echo=TRUE------------------------------------------------------
data_info <- data_available(siteID)
kable(data_info)

## ----salityPlot, echo=TRUE----------------------------------------------------
parameterCd <- c("00095","90095","00940","99220")
site_data <- readNWISqw(siteID,  parameterCd)

Sc_Cl_plot(site_data, title = siteID)


## ----salityTable, echo=TRUE---------------------------------------------------
parameterCd <- c("00095","90095","00940","99220")
site_data <- readNWISqw(siteID,  parameterCd)

sc_cl <- Sc_Cl_table(site_data)

# only show 10 row:
kable(head(sc_cl, 10), 
      col.names = c("Date",
                    "Chloride [mg/L]",
                    "Specific conductance\n
                    microsiemens per liter"))

