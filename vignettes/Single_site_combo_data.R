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

## ----siteInfo, echo=TRUE------------------------------------------------------
library(dataRetrieval)
library(HASP)
library(dplyr)
library(tidyr)

siteID <- "263819081585801"
site_info <- readNWISsite(siteID)

## ----clean_site_info, results='asis'------------------------------------------

# make this a single HASP function: (site_summary?)

site_info_cleaned <- site_info %>% 
  select(site_no, station_nm, lat_va, long_va,
         state_cd, county_cd, huc_cd, 
         nat_aqfr_cd, aqfr_cd,
         land_net_ds,
         well_depth_va,
         alt_va, alt_datum_cd) %>% 
  mutate(state = stateCdLookup(state_cd, 
                               outputType = "fullName"),
         county = countyCdLookup(state = state_cd,
                                 county = county_cd,
                                 outputType = "fullName"),
         nat_aq = nat_aqfr_state$long_name[nat_aqfr_state$nat_aqfr_cd == nat_aqfr_cd],
         local_aq = local_aqfr$Aqfr_Name_prpr[local_aqfr$aqfr_cd == aqfr_cd],
         lat_deg = substr(lat_va, start = 1, stop = 2),
         lat_min = substr(lat_va, start = 3, stop = 4),
         lat_sec = substr(lat_va, start = 5, stop = 6),
         long_deg = substr(long_va, start = 1, stop = 2),
         long_min = substr(long_va, start = 3, stop = 4),
         long_sec = substr(long_va, start = 5, stop = 6))

cat(site_info_cleaned$site_no, site_info_cleaned$station_nm)

cat("<br/>")

cat("Latitude: ", site_info_cleaned$lat_deg, "°",
    site_info_cleaned$lat_min, "'",
    site_info_cleaned$lat_sec, '"', "<br/>")
cat("Longitude: ", site_info_cleaned$long_deg, "°",
    site_info_cleaned$long_min, "'",
    site_info_cleaned$long_sec, '"', "<br/>")
cat(site_info_cleaned$county, ",", site_info_cleaned$state, "<br/>")
cat("Hydrologic Unit: ", site_info_cleaned$huc_cd, "<br/>")
cat("Well depth: ", site_info_cleaned$well_depth_va, " feet<br/>")
cat("Land surface altitude: ", site_info_cleaned$alt_va, " feet above", site_info_cleaned$alt_datum_cd , "<br/>")
cat('Well completed in : "', site_info_cleaned$nat_aq,'" (',
    site_info_cleaned$nat_aqfr_cd, ") national aquifer.<br/>", sep = "")
cat('Well completed in : "', site_info_cleaned$local_aq,'" (',
    site_info_cleaned$aqfr_cd, ") local aquifer.<br/>", sep = "")

## ----whatData, echo=TRUE------------------------------------------------------
data_info <- whatNWISdata(siteNumber = siteID)

## ----whatDataClean------------------------------------------------------------
data_info_clean <- data_info %>% 
  group_by(data_type_cd) %>% 
  summarise(begin = min(begin_date, na.rm = TRUE),
            end = max(end_date, na.rm = TRUE),
            count = max(count_nu, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(`Data Type` = "")


if("uv" %in% data_info_clean$data_type_cd){
  uv_codes <- data_info %>% 
    filter(data_type_cd == "uv") %>% 
    group_by(parm_cd) %>% 
    summarise(begin = min(begin_date, na.rm = TRUE),
              end = max(end_date, na.rm = TRUE),
              count = max(count_nu, na.rm = TRUE))
  data_info_clean$count[data_info_clean$data_type_cd == "uv"] <- NA
  data_info_clean$`Data Type`[data_info_clean$data_type_cd == "uv"] <-  paste0('<a href="https://nwis.waterdata.usgs.gov/nwis/uv?site_no=', siteID, '">Current / Historical Observations</a>')

}

if("gw" %in% data_info_clean$data_type_cd){
  data_info_clean$`Data Type`[data_info_clean$data_type_cd == "gw"] <-  paste0('<a href="https://nwis.waterdata.usgs.gov/nwis/gwlevels?site_no=', siteID, '">Field groundwater-level measurements</a>')
}

if("ad" %in% data_info_clean$data_type_cd){
  data_info_clean$`Data Type`[data_info_clean$data_type_cd == "ad"] <-  paste0('<a href="https://nwis.waterdata.usgs.gov/nwis/wys_rpt?site_no=', siteID, '">Water-Year Summary</a>')    
}

if("qw" %in% data_info_clean$data_type_cd){
  data_info_clean$`Data Type`[data_info_clean$data_type_cd == "qw"] <-  paste0('<a href="https://nwis.waterdata.usgs.gov/nwis/qwdata?site_no=', siteID, '">Field/Lab water-quality samples</a>')  
}


if("dv" %in% data_info_clean$data_type_cd){
  dv_codes <- data_info %>% 
    filter(data_type_cd == "dv") %>% 
    group_by(parm_cd) %>% 
    summarise(begin = min(begin_date, na.rm = TRUE),
              end = max(end_date, na.rm = TRUE),
              count = max(count_nu, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(`Data Type` = readNWISpCode(parm_cd)[["parameter_nm"]]) %>% 
    select(-parm_cd)


    data_info_clean$`Data Type`[data_info_clean$data_type_cd == "dv"] <-  paste0('<a href="https://nwis.waterdata.usgs.gov/nwis/dv?site_no=', siteID, '">Daily Data</a>')
    
    data_info_clean$begin[data_info_clean$data_type_cd == "dv"] <- NA
    data_info_clean$end[data_info_clean$data_type_cd == "dv"] <- NA
    data_info_clean$count[data_info_clean$data_type_cd == "dv"] <- NA
    
    rows_to_dv <- which(data_info_clean$data_type_cd == "dv")
    
    if(length(rows_to_dv) > 0){
      insert_row <- rows_to_dv + 1
      
      if(rows_to_dv == 1){
        data_info_clean_new <- data_info_clean[1,] %>% 
          bind_rows(dv_codes) %>% 
          bind_rows(data_info_clean[(rows_to_dv + nrow(dv_codes)):nrow(data_info_clean),])        
      } else {
        data_info_clean_new <- data_info_clean[1:rows_to_dv,] %>% 
          bind_rows(dv_codes) %>% 
          bind_rows(data_info_clean[(rows_to_dv + nrow(dv_codes)):nrow(data_info_clean),])
        
        data_info_clean <- data_info_clean_new
      }
      
    }
}


data_info_clean <- data_info_clean %>% 
  select(`Data Type`, 
         `Begin Date` = begin, 
         `End Date` = end,
         Count = count)

kable(data_info_clean)


## ----salityStuff--------------------------------------------------------------
parameterCd <- c("00095","90095","00940","99220")
site_data <- readNWISqw(siteID,  parameterCd)

Sc_Cl_plot(site_data, title = siteID)


