
#' get_aquifer_data
#'
#' Get USGS data based on aquiferCd
#' 
#' @param aquiferCd character. To see valid aquifer codes, see the included data
#'  frame \code{local_aqfr}. 
#' @param startDate date or string. Beginning date of when to pull data.
#' @param endDate date of string  Ending date to pull data.
#' @param parameter_cd 5-digit character USGS parameter code.
#' @export
#'
#' @examples 
#' end_date <- "2021-01-01"
#' start_date <- "1989-12-31"
#'
#' aquiferCd <- "S100CSLLWD"
#' \donttest{
#' # aq_data <- get_aquifer_data(aquiferCd, start_date, end_date)
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
      state_data_sites <- attr(state_data, "siteInfo")
      
      state_data_sites <- state_data_sites |> 
        dplyr::select(station_nm, site_no, dec_lat_va, dec_long_va)
      
      aquifer_data <- dplyr::bind_rows(aquifer_data, state_data)
      site_data <- dplyr::bind_rows(site_data, state_data_sites)
    }
    
  }
  
  attr(aquifer_data, "siteInfo") <- site_data
  
  return(aquifer_data)
  
}


#' get_state_data
#'
#' Get USGS data based for a single state with specific aquifer codes.
#' 
#' @param state character. Can be state abbreviation, long name, or numeric code.
#' @param aquiferCd character. To see valid aquifer codes, see the included data
#'  frame \code{local_aqfr}.
#' @param startDate date or string. Beginning date of when to pull data.
#' @param endDate date of string  Ending date to pull data.
#' @param parameter_cd 5-digit character USGS parameter code. Default is "72019".
#' @export
#'
#' @examples 
#' end_date <- "2021-01-01"
#' start_date <- "1989-12-31"
#' aquiferCd <- "S100CSLLWD"
#'
#' \donttest{
#' # st_data <- get_state_data("WI", aquiferCd,
#' #                           start_date, end_date)
#' }
get_state_data <- function(state, aquiferCd, 
                           startDate, endDate, 
                           parameter_cd = "72019"){

  levels <- dataRetrieval::readNWISdata(stateCd = state, 
                         service = "gwlevels",
                         startDate= startDate,
                         endDate = endDate,
                         aquiferCd = aquiferCd,
                         format = "rdb,3.0")

  levels_dv <- dataRetrieval::readNWISdata(stateCd = state, 
                         service = "dv",
                         statCd = "00003",
                         startDate= startDate,
                         endDate = endDate,
                         aquiferCd = aquiferCd)
  
  site_info <- dataRetrieval::whatNWISdata(stateCd = state, 
                                            startDate= startDate,
                                            endDate = endDate,
                                           service = "gwlevels")
  
  if(nrow(levels) + nrow(levels_dv) == 0){
    return(data.frame())
  }

  if(nrow(levels) > 0){

    state_data <- levels |> 
      dplyr::filter(lev_age_cd == "A") |> 
      dplyr::select(lev_dt, site_no, parameter_cd, lev_va, sl_lev_va) |>
      dplyr::mutate(value = dplyr::case_when(is.na(lev_va) ~ sl_lev_va,
                                             TRUE ~ lev_va),
                    state_call = state,
                    year = as.integer(format(as.Date(lev_dt), "%Y")),
                    water_year = water_year(lev_dt),
                    lev_dt = as.Date(lev_dt)) |>
      dplyr::select(-lev_va, -sl_lev_va)
    
  } else {
    state_data <- data.frame()
  }
  
  if(nrow(levels_dv) > 0){

    state_dv <- levels_dv |> 
      dplyr::mutate(year = as.numeric(format(dateTime, "%Y")),
                    water_year = water_year(dateTime),
                    dateTime = as.character(as.Date(dateTime)),
                    state_call = state,
                    lev_dt = as.Date(dateTime)) 
    
    cds <- which(!grepl("_cd", names(state_dv)) &
      !names(state_dv) %in% c("agency_cd", "site_no", "water_year",
                              "dateTime", "tz_cd", "year",
                              "state_call", "lev_dt"))
    names(state_dv)[cds] <- sprintf("%s_value", names(state_dv)[cds])
    
    state_dv <- state_dv |>
      tidyr::pivot_longer(cols = c(-agency_cd, -site_no, -water_year,
                                   -dateTime, -tz_cd, -year,
                                   -state_call, -lev_dt), 
                   names_to = c("pcode", ".value"),
                   names_pattern = "(.+)_(.+)") |>
      dplyr::mutate(pcode = gsub("X_", "", pcode),
                    pcode = substr(pcode, 1, 5)) |>
      dplyr::rename(lev_status_cd = cd,
                    parameter_cd = pcode) |>
      dplyr::filter(lev_status_cd == "A") |>
      dplyr::select(-dateTime, -tz_cd, -agency_cd, -lev_status_cd)
      
  } else {
    state_dv = data.frame()
  }
  
  state_data_tots <- dplyr::bind_rows(state_data, 
                               state_dv)
  
  site_info <- site_info |> 
    dplyr::filter(site_no %in% unique(state_data_tots$site_no))
  
  attr(state_data_tots, "siteInfo") <- site_info
  
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
#' siteID <- "USGS-263819081585801"
#' site_metadata <- site_summary(siteID)
site_summary <- function(siteID, markdown = FALSE){

  site_info <- dataRetrieval::read_waterdata_monitoring_location(monitoring_location_id = siteID)
  
  if(!any(grepl("GW", site_info$site_type_code))){
    warning("Site is not identified as a groundwater site")
    return(site_info)
  }
  
  end_of_line <- ifelse(markdown, "<br/>", "\n\n")
  
  nat_aqfrs <- nat_aqfr_state |> 
    dplyr::select(dplyr::all_of(c("nat_aqfr_cd", "long_name"))) |> 
    dplyr::distinct()
  
  names(nat_aqfrs)[names(nat_aqfrs) == "long_name"] <- "nat_aq"
  
  site_info_cleaned <- site_info |> 
    dplyr::select(dplyr::all_of(c("monitoring_location_id", 
                                  "monitoring_location_name", 
                                  "geometry",
                                  "site_type_code", 
                                  "state_code", 
                                  "state_name",
                                  "county_code",
                                  "county_name",
                                  "hydrologic_unit_code", 
                                  "national_aquifer_code", 
                                  "aquifer_type_code", 
                                  "well_constructed_depth",
                                  "altitude", 
                                  "altitude_method_name"))) |> 
    dplyr::left_join(nat_aqfrs, by = c("national_aquifer_code" = "nat_aqfr_cd")) |> 
    dplyr::left_join(dplyr::rename(local_aqfr, 
                     local_aq = Aqfr_Name_prpr), 
                     by = c("aquifer_type_code" = "aqfr_cd")) 
  
  cat(site_info_cleaned$monitoring_location_id, site_info_cleaned$monitoring_location_name, end_of_line)

  cat(site_info_cleaned$county_name, ",", site_info_cleaned$state_name, end_of_line)
  cat("Hydrologic Unit: ", site_info_cleaned$hydrologic_unit_code, end_of_line)
  cat("Well depth: ", site_info_cleaned$well_constructed_depth, " feet",end_of_line)
  cat("Land surface altitude: ", site_info_cleaned$altitude, site_info_cleaned$altitude_method_name , end_of_line)
  cat('Well completed in : "', site_info_cleaned$nat_aq,'" (',
      site_info_cleaned$national_aquifer_code, ") national aquifer.", end_of_line, sep = "")

  return(site_info_cleaned)
}


#' site_summary
#'
#' Get station summary information
#' 
#' @param siteID character. USGS site ID for a groundwater site.
#' @export
#'
#' @examples 
#' siteID <- "USGS-263819081585801"
#' site_data_available <- data_available(siteID)
data_available <- function(siteID){

  data_info <- dataRetrieval::read_waterdata_ts_meta(monitoring_location_id = siteID,
                                                     skipGeometry = TRUE)
  
  data_info_clean <- data_info |> 
    dplyr::select(`Data Type` = computation_period_identifier, 
                  parameter_name, 
                  parameter_code,
                  statistic_id,
                  `Begin Date` = begin,
                  `End Date` = end,
                  time_series_id) 
  
  return(data_info_clean)
  
}
