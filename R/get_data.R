
#' get_aquifer_data
#'
#' Get USGS data based on aquiferCd
#' 
#' @param aquiferCd character
#' @param startDate date or string
#' @param endDate date of string 
#' @export
#' @import dataRetrieval
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

