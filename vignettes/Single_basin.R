## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE,
                      warning = FALSE,
                      message = FALSE,
                      fig.width = 7,
                      fig.height = 7)
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)

## ----setupData, echo=FALSE----------------------------------------------------
library(compHydros)

aquifer_long_name <- "Basin and Range basin-fill aquifers"

aquiferCd <- summary_aquifers$nat_aqfr_cd[summary_aquifers$long_name == aquifer_long_name]

states <- unlist(summary_aquifers$states[summary_aquifers$long_name == aquifer_long_name])

cat("Aquifer code: ", aquiferCd)
cat("In states: ", paste(states, collapse = ", ") )



## ----dataRetrival-------------------------------------------------------------
library(compHydros)
end_date <- "2019-12-31"
state_date <- "1989-12-31"

aquifer_data <- get_aquifer_data(aquiferCd, 
                                 state_date, 
                                 end_date)


## ----mapIt--------------------------------------------------------------------

map <- map_hydro_data(aquifer_data, "lev_va", 30)
map


## ----graphIt------------------------------------------------------------------

plot_composite_data(aquifer_data, "lev_va", 30) +
    ggtitle(label = aquifer_long_name)


## ----norm---------------------------------------------------------------------

plot_normalized_data(aquifer_data, "lev_va", 30) +
  ggtitle(label = aquifer_long_name)

