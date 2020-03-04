## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE,
                      warning = FALSE,
                      message = FALSE,
                      fig.width = 7,
                      fig.height = 7)

library(HASP)
library(kableExtra)
library(dplyr)


## ----setupData, echo=FALSE----------------------------------------------------


summary_aquifers_disp <- summary_aquifers %>% 
  select(long_name, aquiferCd=nat_aqfr_cd)

kable(summary_aquifers_disp) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>% 
  column_spec(1, width = "50em")


