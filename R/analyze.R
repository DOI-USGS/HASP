
#' site_data_summary
#'
#' Get summaries of data by site. Requires a column site_no, and will
#' take the summaries
#' 
#' @param x data frame
#' @param value_col name of value column. The default is \code{"value"}.
#' @param site_col name of site column. This is the column we are grouping by.
#' @return data frame with 10 columns 
#' @export
#' @importFrom stats quantile
#'
#' @examples 
#' aquifer_data <- aquifer_data
#' aquifer_data <- aquifer_data[aquifer_data$parameter_cd == "72019", ]
#' summary_info <- site_data_summary(aquifer_data)
site_data_summary <- function(x,
                              value_col = "value",
                              site_col = "monitoring_location_id"){

  site_no <- value <- ".dplyr"
  
  if(nrow(x) == 0) stop("No data")
  
  if(!all(c(site_col, value_col) %in% names(x))) stop("Missing columns")

  x$val <- x[[value_col]]
  x$site <- x[[site_col]]
  
  summaries <- dplyr::group_by(x, site)
  
  summaries <- dplyr::summarise(summaries,
                         min_site = suppressWarnings(min(val, na.rm = TRUE)),
                         max_site = suppressWarnings(max(val, na.rm = TRUE)),
                         mean_site = suppressWarnings(mean(val, na.rm = TRUE)),
                         p10 = suppressWarnings(stats::quantile(val, probs = 0.1, na.rm = TRUE)),
                         p25 = suppressWarnings(stats::quantile(val, probs = 0.25, na.rm = TRUE)),
                         p50 = suppressWarnings(stats::quantile(val, probs = 0.5, na.rm = TRUE)),
                         p75 = suppressWarnings(stats::quantile(val, probs = 0.75, na.rm = TRUE)),
                         p90 = suppressWarnings(stats::quantile(val, probs = 0.90, na.rm = TRUE)),
                         count = dplyr::n())
  
  summaries <- dplyr::ungroup(summaries)
  return(summaries)
  
}


#' prep_map_data
#'
#' Get map info
#' 
#' @param x aquifer data
#' @return data frame 
#' @export
#' @keywords internal
#'
#' @examples 
#' aquifer_data <- aquifer_data
#' map_info <- prep_map_data(aquifer_data)
prep_map_data <- function(x ){

  
  if(nrow(x) == 0) stop("No data")
  
  if(!("siteInfo" %in% names(attributes(x)))) stop("Missing site attributes")

  sites <- attr(x, "siteInfo")
  
  map_data <- sites %>%
    dplyr::mutate(popup = paste0('<b><a href="https://waterdata.usgs.gov/monitoring-location/',
                              site_no,'">',
                              site_no,"</a></b><br/>
             <table>
             <tr><td>Name:</td><td>",
                              station_nm,
                              '</td></tr>
             </table>')) %>% 
    dplyr::filter(!is.na(dec_lat_va))
  
  return(map_data)
  
}
  
#' filter_sites
#'
#' Filter down to sites with num_years of data
#' 
#' @param x aquifer data
#' @param num_years integer number of years required. This can be
#' \code{NA}, in which case the filter will use the full range of the data.
#' @param start_year integer the first year to filter from. If \code{NA},
#' the filter will use the minimum from the data.
#' @param end_year integer the last year to filter from. If \code{NA},
#' the filter will use the last year.
#' @param parameter_cd character, 5-digit parameter code, default is "72019".
#' @return data frame filter of x
#' @export
#' @examples 
#' aquifer_data <- aquifer_data
#' num_years <- 30
#' 
#' aq_data <- filter_sites(aquifer_data,
#'                         parameter_cd = "72019",
#'                         num_years = num_years)
filter_sites <- function(x,
                         parameter_cd = "72019",
                         num_years = NA, 
                         start_year = NA, 
                         end_year = NA){
  
  if(nrow(x) == 0) stop("No data")
  
  if(!all(c("site_no", "year", "value") %in% names(x))) stop("Missing columns")

  lev_va <- site_no <- year <- value <- n_years <- ".dplyr"

  pick_sites <- x[x$parameter_cd == parameter_cd, ]
  
  if(nrow(pick_sites) == 0){
    warning("No data with requested parameter code.")
    return(data.frame())
  }
  
  pick_sites <- pick_sites %>%
    dplyr::filter(!is.na(value)) %>% 
    dplyr::group_by(site_no, year) %>% 
    dplyr::summarize(n_meas = dplyr::n()) %>% 
    dplyr::ungroup() 

  
  #if the user doesn't define start/end, use the whole thing
  if(is.na(start_year)){
    start_year <- min(pick_sites$year, na.rm = TRUE)
  }
  
  if(is.na(end_year)){
    # Need to figure out how to check if the last year is complete:
    end_year <- max(pick_sites$year, na.rm = TRUE) - 1
  }
  
  if(is.na(num_years)){
    num_years <- end_year - start_year
  }
  
  if(num_years > end_year +1 - start_year){
    num_years <- end_year - start_year
    warning("Supplied num_years was more than the data range.\nSwitching to num_year = ", num_years)
  }

  if(num_years < end_year - start_year){
    start_year <- end_year - num_years
  }
  
  tots <- expand.grid(year = start_year:end_year,
              site_no = unique(pick_sites$site_no), stringsAsFactors = FALSE) %>% 
    data.frame()
  
  pick_sites_comp <- pick_sites %>% 
    dplyr::right_join(tots, by = c("year", "site_no")) %>% 
    dplyr::filter(year >= start_year,
                  year <= end_year)
  
  sites_incomplete <- unique(pick_sites_comp$site_no[is.na(pick_sites_comp$n_meas)])
  sites_complete <- unique(pick_sites_comp$site_no)
  sites_complete <- sites_complete[!sites_complete %in% sites_incomplete]
  
  # If no sites are complete...we could walk back until there are some 
  # complete sets?
  if(length(sites_complete) == 0){
    warning("No sites had a complete data set")
  }
  
  pick_sites_comp_sum <- pick_sites_comp %>% 
    dplyr::filter(site_no %in% sites_complete) %>% 
    dplyr::group_by(site_no) %>% 
    dplyr::summarise(n_years = length(unique(year))) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(n_years >= !!num_years) %>% 
    dplyr::pull(site_no)
    
  aquifer_data <- x %>% 
    dplyr::filter(site_no %in% pick_sites_comp_sum) %>% 
    dplyr::filter(year >= start_year,
           year <= end_year)
  
  if("siteInfo" %in% names(attributes(x))){
    siteInfo <- attr(x, "siteInfo") %>% 
      dplyr::filter(site_no %in% pick_sites_comp_sum)
    
    attr(aquifer_data, "siteInfo") <- siteInfo    
  }
   
  return(aquifer_data)
  
}

#' Composite hydrograph data
#'
#' Create composite data
#' 
#' @param x aquifer data
#' @param num_years integer number of years required
#' @param parameter_cd character, 5-digit parameter code, default is "72019".
#' @return data frame with year, name, and value
#' 
#' @export
#' @examples 
#' aquifer_data <- aquifer_data
#' num_years <- 30
#' 
#' comp_data <- composite_data(aquifer_data, num_years, "72019")
#' 
composite_data <- function(x, num_years, parameter_cd){
  
  year <- site_no <- n_sites_year <- med_site <- value <- name <- ".dplyr"
  
  if(nrow(x) == 0) stop("No data")
  
  if(!all(c("site_no", "year", "value") %in% names(x))) stop("Missing columns")

  x <- filter_sites(x, num_years, parameter_cd = parameter_cd)
  
  if(nrow(x) == 0){
    stop("No data ")
  }
  
  n_sites <- length(unique(x$site_no))
  
  composite <- x %>% 
    dplyr::group_by(year, site_no) %>% 
    dplyr::summarize(med_site = stats::median(value, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::distinct(year, site_no, med_site) %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarise(mean = mean(med_site, na.rm = TRUE),
              median = stats::median(med_site, na.rm = TRUE),
              n_sites_year = length(unique(site_no))) %>% 
    dplyr::filter(n_sites_year == {{n_sites}}) %>%
    dplyr::select(-n_sites_year) %>% 
    tidyr::pivot_longer(c("mean", "median")) %>% 
    dplyr::mutate(name = factor(name, 
                         levels = c("median","mean"),
                         labels = c("Median",
                                    "Mean") ))
  
  attr(composite, "n_sites") <- n_sites
  
  return(composite)
}

#' Composite normalized hydrograph data
#'
#' Create normalized composite data
#' 
#' Information can be found here: \url{https://groundwaterwatch.usgs.gov/composite/help/CompositeGroundwaterLevelHelpDocument.docx.html}
#' 
#' @param x aquifer data
#' @param num_years integer number of years required
#' @param parameter_cd character, 5-digit parameter code, default is "72019".
#' @return data frame with year, name, and value
#' @export
#' @examples 
#' aquifer_data <- aquifer_data
#' num_years <- 30
#' 
#' norm_data <- normalized_data(aquifer_data, num_years, "72019")
normalized_data <- function(x, num_years, parameter_cd = "72019"){

  if(nrow(x) == 0) stop("No data")
  
  if(!all(c("site_no", "year", "value") %in% names(x))) stop("Missing columns")

  if(nrow(x) == 0){
    stop("No data")
  }
  
  x <- filter_sites(x,
                    num_years = num_years, 
                    parameter_cd = parameter_cd)
  n_sites <- length(unique(x$site_no))
  year_summaries <- site_data_summary(x, 
                                      value_col = "value", site_col = "site_no")
  
  norm_composite <- x %>% 
    dplyr::group_by(year, site_no) %>% 
    dplyr::mutate(med_site = stats::median(value, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::distinct(year, site_no, med_site) %>% 
    dplyr::group_by(site_no) %>% 
    dplyr::mutate(max_med = max(med_site, na.rm = TRUE),
           min_med = min(med_site, na.rm = TRUE),
           mean_med = mean(med_site, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(x_norm = -1*(med_site - mean_med)/(max_med - min_med)) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarise(mean = mean(x_norm, na.rm = TRUE),
              median = stats::median(x_norm, na.rm = TRUE),
              n_sites_year = length(unique(site_no))) %>% 
    dplyr::filter(!n_sites_year < {{n_sites}}) %>% 
    dplyr::select(-n_sites_year) %>% 
    tidyr::pivot_longer(c("mean", "median")) %>% 
    dplyr::mutate(name = factor(name, 
                         levels = c("median","mean"),
                         labels = c("Median",
                                    "Mean") ))
  
  attr(norm_composite, "n_sites") <- n_sites
  return(norm_composite)
}

#' Convert to water year
#' 
#' This function is a little more robust than \code{\link[dataRetrieval]{calcWaterYear}}
#' 
#' @param x character vector
#' @export
#' 
#' @examples 
#' x <- c("2010-01-01", "1994-02", "1980", "2009-11-01")
#' water_year(x)
water_year <- function(x){
  
  x_date <- as.Date(x)
  
  if(any(is.na(x_date))){
    bad_dates <- x[which(is.na(x_date))]
    
    # Year-month date:
    # this one is legit....the day will never affect the water year:
    x[grep("^(\\d{4}-\\d{2}$)", x)] <- paste0(x[grep("^(\\d{4}-\\d{2}$)", x)],"-01")
    
    if(length(grep("^(\\d{4}$)", x)) > 0){
      message("Calendar year being reported as water year in row(s) ", paste(grep("^(\\d{4}$)", x), collapse = ", "))
      # this one is less legit...maybe USGS only reports in water years?
      x[grep("^(\\d{4}$)", x)] <- paste0(x[grep("^(\\d{4}$)", x)],"-01-01")
    }
    
    x_date <- as.Date(x)
  }
  
  return(dataRetrieval::calcWaterYear(x_date))
  
}

