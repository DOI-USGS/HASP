
#' get_aquifer_data
#'
#' Get USGS data based on aquiferCd
#' 
#' @param aquiferCd character
#' @param startDate date or string
#' @param endDate date of string 
#' @export
#' @importFrom dataRetrieval readNWISdata
#' @importFrom dataRetrieval readNWISsite
#' @import dplyr
#'
#' @examples 
#' end_date <- "2019-12-31"
#' start_date <- "1989-12-31"
#'
#' aquiferCd <- "S100CSLLWD"
#' aq_data <- get_aquifer_data(aquiferCd, start_date, end_date)
get_aquifer_data <- function(aquiferCd, startDate, endDate){
  
  aquifer_data <- data.frame()
  site_data <- data.frame()
  
  station_nm <- site_no <- dec_lat_va <- dec_long_va <- ".dplyr"
  
  states <- unlist(summary_aquifers$states[summary_aquifers$nat_aqfr_cd == aquiferCd])
  
  for(state in states){
    
    message("Getting data from: ", state)
    state_data <- tryCatch(
      expr = {
        get_state_data(state = state, 
                       aquiferCd = aquiferCd, 
                       startDate = startDate,
                       endDate = endDate)
      }, 
      error = function(e){ 
        cat(state, "errored \n")
      }
    )
    
    if(inherits(state_data, "error")) next
    
    if(!all(is.na(state_data$site_no))){
      state_data_sites <- readNWISsite(unique(state_data$site_no))
      
      state_data_sites <- state_data_sites %>% 
        select(station_nm, site_no, dec_lat_va, dec_long_va)
      
      aquifer_data <- bind_rows(aquifer_data, state_data)
      site_data <- bind_rows(site_data, state_data_sites)
    }
    
  }
  
  attr(aquifer_data, "siteInfo") <- site_data
  
  return(aquifer_data)
  
}


get_state_data <- function(state, aquiferCd, startDate, endDate){

  levels <- readNWISdata(stateCd = state, 
                         service = "gwlevels",
                         startDate= startDate,
                         endDate = endDate,
                         aquiferCd = aquiferCd)
  
  state_data <- levels[ , c("lev_va", "sl_lev_va", "lev_dt", "site_no")]

  state_data$state_call <- state
  state_data$lev_va <- as.numeric(state_data$lev_va)
  state_data$sl_lev_va <- as.numeric(state_data$sl_lev_va)
  state_data$lev_dt <- as.character(state_data$lev_dt)
  
  state_data$year <- as.numeric(sapply(strsplit(state_data$lev_dt, 
                                                split = "-"), 
                                       function(x) x[1]))
  
  
  return(state_data)
}

#' site_summary
#'
#' Get station summary information
#' 
#' @param siteID character
#' @param markdown logical. Use markdown formating or console-friendly.
#' @export
#' @importFrom dataRetrieval readNWISsite
#' @importFrom dataRetrieval stateCdLookup
#' @importFrom dataRetrieval countyCdLookup
#' @import dplyr
#'
#' @examples 
#' siteID <- "263819081585801"
#' site_metadata <- site_summary(siteID)
site_summary <- function(siteID, markdown = FALSE){
  
  site_info <- readNWISsite(siteID)
  
  end_of_line <- ifelse(markdown, "<br/>", "\n")
  
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
  
  cat(site_info_cleaned$site_no, site_info_cleaned$station_nm, end_of_line)

  cat("Latitude: ", site_info_cleaned$lat_deg, "deg",
      site_info_cleaned$lat_min, "'",
      site_info_cleaned$lat_sec, '"', end_of_line)
  cat("Longitude: ", site_info_cleaned$long_deg, "deg",
      site_info_cleaned$long_min, "'",
      site_info_cleaned$long_sec, '"', end_of_line)
  cat(site_info_cleaned$county, ",", site_info_cleaned$state, end_of_line)
  cat("Hydrologic Unit: ", site_info_cleaned$huc_cd, end_of_line)
  cat("Well depth: ", site_info_cleaned$well_depth_va, " feet",end_of_line)
  cat("Land surface altitude: ", site_info_cleaned$alt_va, " feet above", site_info_cleaned$alt_datum_cd , end_of_line)
  cat('Well completed in : "', site_info_cleaned$nat_aq,'" (',
      site_info_cleaned$nat_aqfr_cd, ") national aquifer.", end_of_line, sep = "")
  cat('Well completed in : "', site_info_cleaned$local_aq,'" (',
      site_info_cleaned$aqfr_cd, ") local aquifer.",end_of_line, sep = "")
  return(site_info_cleaned)
}


#' site_summary
#'
#' Get station summary information
#' 
#' @param siteID character
#' @param markdown logical. Use markdown formating or console-friendly.
#' @export
#' @importFrom dataRetrieval whatNWISdata
#' @importFrom dataRetrieval stateCdLookup
#' @importFrom dataRetrieval countyCdLookup
#' @import dplyr
#'
#' @examples 
#' siteID <- "263819081585801"
#' site_data_available <- data_available(siteID)
data_available <- function(siteID){
  
  data_type_cd <- begin_date <- end_date <- count_nu <- `Data Type` <- parm_cd <- ".dplyr"
  data_info <- whatNWISdata(siteNumber = siteID)
  
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
    #TODO: add something similar to dv
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
  
  return(data_info_clean)
  
}