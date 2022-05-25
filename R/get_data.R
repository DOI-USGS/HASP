
#' get_aquifer_data
#'
#' Get USGS data based on aquiferCd
#' 
#' @param aquiferCd character
#' @param startDate date or string
#' @param endDate date of string 
#' @param parameter_cd 5-digit character USGS parameter code.
#' @export
#'
#' @examples 
#' end_date <- "2021-01-01"
#' start_date <- "1989-12-31"
#'
#' aquiferCd <- "S100CSLLWD"
#' \donttest{
#' aq_data <- get_aquifer_data(aquiferCd, start_date, end_date)
#' }
get_aquifer_data <- function(aquiferCd, startDate, endDate, 
                             parameter_cd = "72019"){
  
  aquifer_data <- data.frame()
  site_data <- data.frame()

  states <- unlist(summary_aquifers$states[summary_aquifers$nat_aqfr_cd == aquiferCd])
  
  for(state in states){
    
    message("Getting data from: ", state)
    state_data <- tryCatch(
      expr = {
        get_state_data(state = state, 
                       aquiferCd = aquiferCd, 
                       startDate = startDate,
                       endDate = endDate,
                       parameter_cd = parameter_cd)
      }, 
      error = function(e){ 
        cat(state, "errored \n")
      }
    )
    
    if(inherits(state_data, "error")) next
    
    if(!all(is.na(state_data$site_no))){
      state_data_sites <- dataRetrieval::readNWISsite(unique(state_data$site_no))
      
      state_data_sites <- state_data_sites %>% 
        dplyr::select(station_nm, site_no, dec_lat_va, dec_long_va)
      
      aquifer_data <- dplyr::bind_rows(aquifer_data, state_data)
      site_data <- dplyr::bind_rows(site_data, state_data_sites)
    }
    
  }
  
  attr(aquifer_data, "siteInfo") <- site_data
  
  return(aquifer_data)
  
}


get_state_data <- function(state, aquiferCd, 
                           startDate, endDate, parameter_cd){

  levels <- dataRetrieval::readNWISdata(stateCd = state, 
                         parameterCd = parameter_cd,
                         service = "gwlevels",
                         startDate= startDate,
                         endDate = endDate,
                         aquiferCd = aquiferCd)
  
  levels_dv <- dataRetrieval::readNWISdata(stateCd = state, 
                         service = "dv",
                         parameterCd = parameter_cd,
                         statCd = "00003",
                         startDate= startDate,
                         endDate = endDate,
                         aquiferCd = aquiferCd)
  val_col <- ifelse(parameter_cd == "72019", "lev_va", "sl_lev_va")
  
  if(nrow(levels) > 0){
    state_data <- levels[ , c(val_col, "lev_dt", "site_no", "lev_age_cd")]
    
    state_data <- levels %>% 
      dplyr::filter(lev_age_cd == "A") %>% 
      dplyr::select(lev_dt, site_no, dplyr::all_of(val_col)) 
    
    state_data$state_call <- state
    state_data$value <- state_data[[val_col]]
    state_data$lev_dt <- as.character(state_data$lev_dt)
    
    state_data <- state_data %>%
      dplyr::mutate(year = as.numeric(sapply(strsplit(lev_dt, 
                                               split = "-"), 
                                      function(x) x[1])),
             water_year = water_year(lev_dt),
             lev_dt = as.Date(lev_dt)) %>% 
      dplyr::select(-dplyr::all_of(val_col))
  } else {
    state_data <- data.frame()
  }
  
  if(nrow(levels_dv) > 0){
    val_col_dv <- paste0("X_", parameter_cd, "_00003")
    
    levels_dv$result <- levels_dv[[val_col_dv]]
    state_dv <- levels_dv %>% 
      dplyr::mutate(year = as.numeric(format(dateTime, "%Y")),
                    water_year = water_year(dateTime),
                    dateTime = as.character(as.Date(dateTime)),
                    state_call = state,
                    value = as.numeric(result),
                    lev_dt = as.Date(dateTime)) %>% 
      dplyr::select(dplyr::all_of(c("lev_dt", "site_no", "state_call", "value",
                                    "year", "water_year")))
    
  } else {
    state_dv = data.frame()
  }
  

  state_data_tots <- dplyr::bind_rows(state_data, 
                               state_dv)
  
  return(state_data_tots)
}

#' site_summary
#'
#' Get station summary information
#' 
#' @param siteID character
#' @param markdown logical. Use markdown formating or console-friendly.
#' @export
#'
#' @examples 
#' siteID <- "263819081585801"
#' site_metadata <- site_summary(siteID)
site_summary <- function(siteID, markdown = FALSE){

  site_info <- dataRetrieval::readNWISsite(siteID)
  
  if(!any(grepl("GW", site_info$site_tp_cd))){
    warning("Site is not identified as a groundwater site")
    return(site_info)
  }
  
  end_of_line <- ifelse(markdown, "<br/>", "\n")
  
  nat_aqfrs <- nat_aqfr_state %>% 
    dplyr::select(dplyr::all_of(c("nat_aqfr_cd", "long_name"))) %>% 
    dplyr::distinct()
  
  names(nat_aqfrs)[names(nat_aqfrs) == "long_name"] <- "nat_aq"
  
  site_info_cleaned <- site_info %>% 
    dplyr::select(dplyr::all_of(c("site_no", "station_nm", "lat_va", "long_va",
           "site_tp_cd", "state_cd", "county_cd", "huc_cd", 
           "nat_aqfr_cd", "aqfr_cd", "land_net_ds", "well_depth_va",
           "alt_va", "alt_datum_cd"))) %>% 
    dplyr::left_join(nat_aqfrs, by = "nat_aqfr_cd") %>% 
    dplyr::left_join(dplyr::rename(local_aqfr, 
                     local_aq = Aqfr_Name_prpr), by = "aqfr_cd") %>% 
    dplyr::mutate(state = dataRetrieval::stateCdLookup(state_cd, 
                                 outputType = "fullName"),
           county = dataRetrieval::countyCdLookup(state = state_cd,
                                   county = county_cd,
                                   outputType = "fullName"),
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
#' @export
#'
#' @examples 
#' siteID <- "263819081585801"
#' site_data_available <- data_available(siteID)
data_available <- function(siteID){

  data_info <- dataRetrieval::whatNWISdata(siteNumber = siteID)
  
  data_info_clean <- data_info %>% 
    dplyr::group_by(data_type_cd) %>% 
    dplyr::summarise(begin = min(begin_date, na.rm = TRUE),
              end = max(end_date, na.rm = TRUE),
              count = max(count_nu, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(`Data Type` = "")
  
  if("uv" %in% data_info_clean$data_type_cd){
    uv_codes <- data_info %>% 
      dplyr::filter(data_type_cd == "uv") %>% 
      dplyr::group_by(parm_cd) %>% 
      dplyr::summarise(begin = min(begin_date, na.rm = TRUE),
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
      dplyr::filter(data_type_cd == "dv") %>% 
      dplyr::group_by(parm_cd) %>% 
      dplyr::summarise(begin = min(begin_date, na.rm = TRUE),
                end = max(end_date, na.rm = TRUE),
                count = max(count_nu, na.rm = TRUE)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(`Data Type` = dataRetrieval::readNWISpCode(parm_cd)[["parameter_nm"]]) %>% 
      dplyr::select(-parm_cd)
    
    data_info_clean$`Data Type`[data_info_clean$data_type_cd == "dv"] <-  paste0('<a href="https://nwis.waterdata.usgs.gov/nwis/dv?site_no=', siteID, '">Daily Data</a>')
    
    data_info_clean$begin[data_info_clean$data_type_cd == "dv"] <- NA
    data_info_clean$end[data_info_clean$data_type_cd == "dv"] <- NA
    data_info_clean$count[data_info_clean$data_type_cd == "dv"] <- NA
    
    rows_to_dv <- which(data_info_clean$data_type_cd == "dv")
    
    if(length(rows_to_dv) > 0){
      insert_row <- rows_to_dv + 1
      
      if(rows_to_dv == 1){
        data_info_clean_new <- data_info_clean[1,] %>% 
          dplyr::bind_rows(dv_codes) %>% 
          dplyr::bind_rows(data_info_clean[(rows_to_dv + nrow(dv_codes)):nrow(data_info_clean),])        
      } else {
        data_info_clean_new <- data_info_clean[1:rows_to_dv,] %>% 
          dplyr::bind_rows(dv_codes) %>% 
          dplyr::bind_rows(data_info_clean[(rows_to_dv + nrow(dv_codes)):nrow(data_info_clean),])
        
        data_info_clean <- data_info_clean_new
      }
      
    }
  }
  
  data_info_clean <- data_info_clean %>% 
    dplyr::select(`Data Type`, 
           `Begin Date` = begin, 
           `End Date` = end,
           Count = count)
  
  return(data_info_clean)
  
}