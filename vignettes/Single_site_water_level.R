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

