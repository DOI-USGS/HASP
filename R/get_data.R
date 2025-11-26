
#' get_aquifer_data
#'
#' Get USGS data based on aquiferCd
#' 
#' @param aquiferCd character. To see valid aquifer codes, see
#' \code{\link[dataRetrieval:read_waterdata_metadata]{dataRetrieval::read_waterdata_metadata()}}
#' \code{aquifer_codes <- dataRetrieval::read_waterdata_metadata("national-aquifer-codes")}.
#' @param startDate date or string. Beginning date of when to pull data.
#' @param endDate date of string  Ending date to pull data.
#' @param parameter_cd 5-digit character USGS parameter code.
#' @param statistic_cd 5-digit character USGS daily statistics code. Default is "00008",
#' @export
#'
#' @examples 
#' end_date <- "2025-01-01"
#' start_date <- "2010-01-01"
#' parameter_cd = "72019"
#' statistic_cd = "00003"
#' aquiferCd <- "N100BSNRGB"
#' \donttest{
#' # aq_data <- get_aquifer_data(aquiferCd, start_date, end_date)
#' }
get_aquifer_data <- function(aquiferCd, 
                             startDate,
                             endDate, 
                             parameter_cd = "72019",
                             statistic_cd = "00003"){
  
  sites <- dataRetrieval::read_waterdata_monitoring_location(national_aquifer_code = aquiferCd)

  levels_dv <- data.frame()
  levels_gwl <- data.frame()
  
  for(state in unique(sites$state_name)){

    appropriate_sites <- dataRetrieval::read_waterdata_ts_meta(state_name = state,
                                                               parameter_code = parameter_cd,
                                                               # statistic_id = statistic_cd,
                                                               computation_period_identifier = "Daily",
                                                               skipGeometry = TRUE,
                                                               properties = c("monitoring_location_id",
                                                                              "parameter_code",
                                                                              "statistic_id",
                                                                              "state_name"))
    
    sites_in_state <- sites$monitoring_location_id[sites$monitoring_location_id %in% appropriate_sites$monitoring_location_id]
    
    if(length(sites_in_state) == 0){
      next
    } else {
    
      for(i in sites_in_state){
        df_gwl <- dataRetrieval::read_waterdata_field_measurements(monitoring_location_id = i,
                                                                   time = c(startDate, endDate),
                                                                   parameter_code = parameter_cd,
                                                                   approval_status = "Approved",
                                                                   skipGeometry = TRUE,
                                                                   properties = c("time",
                                                                                  "monitoring_location_id",
                                                                                  "value"))
        
        df_dv <- dataRetrieval::read_waterdata_daily(monitoring_location_id = i,
                                                     time = c(startDate, endDate),
                                                     parameter_code = parameter_cd,
                                                     approval_status = "Approved",
                                                     statistic_id = statistic_cd,
                                                     skipGeometry = TRUE,
                                                     properties = c("time", 
                                                                    "monitoring_location_id", 
                                                                    "value")) 
        if(nrow(df_dv) > 0){
          levels_dv <- dplyr::bind_rows(levels_dv, df_dv)
        }
        
        if(nrow(df_gwl) > 0){
          levels_gwl <- dplyr::bind_rows(levels_gwl, df_gwl)
        }
      }
    }
    
  }
  
  if(nrow(df_gwl) + nrow(levels_dv) == 0){
    return(data.frame())
  }
  
  if(nrow(df_gwl) > 0){
    aquifer_data <- df_gwl |> 
      dplyr::mutate(year = as.integer(format(as.Date(time), "%Y")),
                    water_year = water_year(time),
                    time = as.Date(time)) 
    
  } else {
    aquifer_data <- data.frame()
  }
  
  if(nrow(levels_dv) > 0){
    aquifer_dv <- levels_dv |> 
      dplyr::mutate(year = as.numeric(format(time, "%Y")),
                    water_year = water_year(time),
                    time = as.Date(time))
    
  } else {
    aquifer_dv = data.frame()
  }
  
  aquifer_data_tots <- dplyr::bind_rows(aquifer_data, 
                                        aquifer_dv)
  
  return(aquifer_data_tots)
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
  
  data_info_clean <- data_info[, c("computation_period_identifier",
                                   "parameter_name", 
                                   "parameter_code",
                                   "statistic_id",
                                   "begin",
                                   "end")]
  
  names(data_info_clean)[names(data_info_clean) == "computation_period_identifier"] <- "Data Type"
  names(data_info_clean)[names(data_info_clean) == "begin"] <- "Begin Date"
  names(data_info_clean)[names(data_info_clean) == "end"] <- "End Date"

  field_info <- dataRetrieval::read_waterdata_field_measurements(monitoring_location_id = siteID,
                                                                 skipGeometry = TRUE)
  if(nrow(field_info) > 0){

    inventory <- stats::aggregate(time ~ parameter_code,
                           data = field_info,
                           FUN = min)
    names(inventory)[2] <- c("Begin Date")
    
    inventory2 <- stats::aggregate(time ~ parameter_code,
                           data = field_info,
                           FUN = max)
    names(inventory2)[2] <- c("End Date")
    
    inventory <- merge(inventory, inventory2, by = "parameter_code")
    inventory$`Data Type` <- "Field"
    inventory$statistic_id <- ""
    
    pcodes <- dataRetrieval::read_waterdata_parameter_codes(parameter_code = unique(inventory$parameter_code))
    pcodes <- pcodes[, c("parameter_code", "parameter_name")]
    
    inventory <- merge(inventory, pcodes, by = "parameter_code")
    
    inventory <- inventory[, names(data_info_clean)]
    
    data_info_clean <- data_info_clean |> 
      rbind(inventory)
  }

  what_qw <- dataRetrieval::summarize_waterdata_samples(monitoringLocationIdentifier = siteID)
  
  if(nrow(what_qw) > 0){

    characteristics <- dataRetrieval::check_waterdata_sample_params("characteristics")
    characteristics <- characteristics[, c("characteristicNameUserSupplied",
                                           "parameterCode")]
    names(characteristics) <- c("parameter_name",
                                "parameter_code")
    characteristics <- characteristics[!is.na(characteristics$parameter_code), ]
    characteristics <- stats::aggregate(parameter_code ~ parameter_name,
                           data = characteristics,
                           FUN = paste0, collapse = ", ")
    
    
    what_qw_cleaned <- what_qw[, c("characteristicUserSupplied",
                                   "firstActivity",
                                   "mostRecentActivity")]
    names(what_qw_cleaned) <- c("parameter_name",
                                "Begin Date",
                                "End Date")
    what_qw_cleaned$`Data Type` <- "Discrete Samples"
      
    what_qw_cleaned <- merge(what_qw_cleaned, 
                             characteristics,
                             by = "parameter_name")
    
    what_qw_cleaned$statistic_id <- ""
    
    data_info_clean <- data_info_clean |> 
      rbind(what_qw_cleaned)
  }
  
  data_info_clean$`Begin Date` <- as.Date(data_info_clean$`Begin Date`)
  data_info_clean$`End Date` <- as.Date(data_info_clean$`End Date`)
  
  return(data_info_clean)
  
}



