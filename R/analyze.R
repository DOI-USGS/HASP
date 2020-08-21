
#' site_data_summary
#'
#' Get summaries of data by site. Requires a column site_no, and will
#' take the summaries on the user-defined column based on the sum_col argument.
#' 
#' @param x data frame
#' @param sum_col column name
#' 
#' @return data frame with 10 columns 
#' @export
#' @import dplyr
#' @importFrom stats median
#' @importFrom stats quantile
#'
#' @examples 
#' aquifer_data <- aquifer_data
#' sum_col <- "lev_va"
#' summary_info <- site_data_summary(aquifer_data, sum_col)
site_data_summary <- function(x, sum_col){

  site_no <- ".dplyr"
  
  if(nrow(x) == 0) stop("No data")
  
  if(!all(c("site_no", sum_col) %in% names(x))) stop("Missing columns")
  
  summaries <- group_by(x, site_no)
  
  summaries <- summarise(summaries,
                         min_site = min(!!sym(sum_col), na.rm = TRUE),
                         max_site = max(!!sym(sum_col), na.rm = TRUE),
                         mean_site = mean(!!sym(sum_col), na.rm = TRUE),
                         p10 = quantile(!!sym(sum_col), probs = 0.1, na.rm = TRUE),
                         p25 = quantile(!!sym(sum_col), probs = 0.25, na.rm = TRUE),
                         p50 = quantile(!!sym(sum_col), probs = 0.5, na.rm = TRUE),
                         p75 = quantile(!!sym(sum_col), probs = 0.75, na.rm = TRUE),
                         p90 = quantile(!!sym(sum_col), probs = 0.90, na.rm = TRUE),
                         count = n())
  
  summaries <- ungroup(summaries)
  return(summaries)
  
}


#' prep_map_data
#'
#' Get map info
#' 
#' @param x aquifer data
#' @param sum_col column name
#' @return data frame 
#' @export
#' @import dplyr
#'
#' @examples 
#' aquifer_data <- aquifer_data
#' map_info <- prep_map_data(aquifer_data)
prep_map_data <- function(x ){
  
  lev_dt <- site_no <- category <- dec_lat_va <- station_nm <- dec_long_va <- ".dplyr"
  
  if(nrow(x) == 0) stop("No data")
  
  if(!("siteInfo" %in% names(attributes(x)))) stop("Missing site attributes")

  sites <- attr(x, "siteInfo")
  
  map_data <- sites %>%
    mutate(popup = paste0('<b><a href="https://waterdata.usgs.gov/monitoring-location/',
                              site_no,'">',
                              site_no,"</a></b><br/>
             <table>
             <tr><td>Name:</td><td>",
                              station_nm,
                              '</td></tr>
             </table>')) %>% 
    filter(!is.na(dec_lat_va))
  
  return(map_data)
  
}
  
#' filter_sites
#'
#' Filter down to sites with num_years of data
#' 
#' @param x aquifer data
#' @param sum_col column name
#' @param num_years integer number of years required
#' @return data frame filter of x
#' @export
#' @examples 
#' aquifer_data <- aquifer_data
#' sum_col <- "lev_va"
#' num_years <- 30
#' 
#' aq_data <- filter_sites(aquifer_data, sum_col, num_years)
filter_sites <- function(x, sum_col, num_years){
  
  if(nrow(x) == 0) stop("No data")
  
  if(!all(c("site_no", "year", sum_col) %in% names(x))) stop("Missing columns")

  lev_va <- site_no <- year <- ".dplyr"

  pick_sites <- x %>% 
    filter(!is.na(!!sym(sum_col))) %>% 
    group_by(site_no, year) %>% 
    summarize(n_meas = n()) %>% 
    ungroup() 
  
  years = data.frame(year = min(pick_sites$year):max(pick_sites$year))
  
  tots <- expand.grid(year = min(pick_sites$year):max(pick_sites$year),
              site_no = unique(pick_sites$site_no), stringsAsFactors = FALSE) %>% 
    data.frame()
  
  pick_sites_comp <- pick_sites %>% 
    right_join(tots, by = c("year", "site_no"))
  
  sites_incomplete <- unique(pick_sites_comp$site_no[is.na(pick_sites_comp$n_meas)])
  sites_complete <- unique(pick_sites_comp$site_no)
  sites_complete <- sites_complete[!sites_complete %in% sites_incomplete]
  
  pick_sites_comp_sum <- pick_sites_comp %>% 
    filter(site_no %in% sites_complete) %>% 
    group_by(site_no) %>% 
    summarise(n_years = length(unique(year))) %>% 
    ungroup() %>% 
    filter(n_years >= !!num_years) %>% 
    pull(site_no)
    
  aquifer_data <- x %>% 
    filter(site_no %in% pick_sites_comp_sum)
  
  if("siteInfo" %in% names(attributes(x))){
    siteInfo <- attr(x, "siteInfo") %>% 
      filter(site_no %in% pick_sites_comp_sum)
    
    attr(aquifer_data, "siteInfo") <- siteInfo    
  }
   
  return(aquifer_data)
  
}

#' Composite hydrograph data
#'
#' Create composite data
#' 
#' @param x aquifer data
#' @param sum_col column name
#' @param num_years integer number of years required
#' @return data frame with year, name, and value
#' 
#' @importFrom tidyr pivot_longer
#' @export
#' @examples 
#' aquifer_data <- aquifer_data
#' sum_col <- "lev_va"
#' num_years <- 30
#' 
#' comp_data <- composite_data(aquifer_data, sum_col, num_years)
#' 
composite_data <- function(x, sum_col, num_years){
  
  year <- site_no <- n_sites_year <- med_site <- name <- ".dplyr"
  
  if(nrow(x) == 0) stop("No data")
  
  if(!all(c("site_no", "year", sum_col) %in% names(x))) stop("Missing columns")

  x <- filter_sites(x, sum_col, num_years)
  
  if(nrow(x) == 0){
    stop("No data in ", sum_col)
  }
  
  n_sites <- length(unique(x$site_no))
  
  composite <- x %>% 
    group_by(year, site_no) %>% 
    summarize(med_site = median(!!sym(sum_col), na.rm = TRUE)) %>% 
    ungroup() %>% 
    distinct(year, site_no, med_site) %>% 
    group_by(year) %>% 
    summarise(mean = mean(med_site, na.rm = TRUE),
              median = median(med_site, na.rm = TRUE),
              n_sites_year = length(unique(site_no))) %>% 
    filter(n_sites_year == {{n_sites}}) %>%
    select(-n_sites_year) %>% 
    pivot_longer(c("mean", "median")) %>% 
    mutate(name = factor(name, 
                         levels = c("median","mean"),
                         labels = c("Median",
                                    "Mean") ))
  
  
  return(composite)
}

#' Composite normalized hydrograph data
#'
#' Create normalized composite data
#' 
#' Information can be found here: \url{https://groundwaterwatch.usgs.gov/composite/help/CompositeGroundwaterLevelHelpDocument.docx.html}
#' 
#' @param x aquifer data
#' @param sum_col column name
#' @param num_years integer number of years required
#' @return data frame with year, name, and value
#' @importFrom tidyr pivot_longer
#' @export
#' @examples 
#' aquifer_data <- aquifer_data
#' sum_col <- "lev_va"
#' num_years <- 30
#' 
#' norm_data <- normalized_data(aquifer_data, sum_col, num_years)
normalized_data <- function(x, sum_col, num_years){
  
  year <- site_no <- n_sites_year <- mean_site <- max_site <- min_site <- x_norm <- med_site <- name <- ".dplyr"
  mean_med <- max_med <- min_med <- ".dplyr"
  
  if(nrow(x) == 0) stop("No data")
  
  if(!all(c("site_no", "year", sum_col) %in% names(x))) stop("Missing columns")
  
  x <- filter_sites(x, sum_col, num_years)
  
  if(nrow(x) == 0){
    stop("No data in ", sum_col)
  }
  
  n_sites <- length(unique(x$site_no))
  
  year_summaries <- site_data_summary(x, sum_col)
  
  norm_composite <- x %>% 
    group_by(year, site_no) %>% 
    mutate(med_site = median(!!sym(sum_col), na.rm = TRUE)) %>% 
    ungroup() %>% 
    distinct(year, site_no, med_site) %>% 
    group_by(site_no) %>% 
    mutate(max_med = max(med_site, na.rm = TRUE),
           min_med = min(med_site, na.rm = TRUE),
           mean_med = mean(med_site, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(x_norm = -1*(med_site - mean_med)/(max_med - min_med)) %>% 
    ungroup() %>% 
    group_by(year) %>% 
    summarise(mean = mean(x_norm, na.rm = TRUE),
              median = median(x_norm, na.rm = TRUE),
              n_sites_year = length(unique(site_no))) %>% 
    filter(!n_sites_year < {{n_sites}}) %>% 
    select(-n_sites_year) %>% 
    pivot_longer(c("mean", "median")) %>% 
    mutate(name = factor(name, 
                         levels = c("median","mean"),
                         labels = c("Median",
                                    "Mean") ))
  
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

