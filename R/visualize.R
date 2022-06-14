#' Plot composite hydrograph data
#'
#' Create composite hydrograph plot
#' 
#' @param x aquifer data frame. Requires at least 3 columns. Two are required "site_no", "year",
#' and "value".
#' @param num_years integer number of years required. If \code{NA}, the 
#' analysis will default to the range of the data in x.
#' @param plot_title character title included on plot.
#' @param parameter_cd character, 5-digit parameter code, default is "72019".
#' @return ggplot2 object
#' 
#' @import ggplot2
#' @export
#' @examples 
#' aquifer_data <- aquifer_data
#' 
#' comp_data <- plot_composite_data(aquifer_data, 
#'                                  num_years = 30, 
#'                                  parameter_cd = "72019",
#'                                  plot_title = "Calendar Year")
#' comp_data
#' # Do it on a water year:
#' aquifer_data$cal_year <- aquifer_data$year
#' aquifer_data$year <- aquifer_data$water_year
#' plot_composite_data(aquifer_data, num_years, 
#'                     parameter_cd = "72019",
#'                     plot_title = "Water Year")
plot_composite_data <- function(x, 
                                num_years = NA, 
                                parameter_cd = "72019",
                                plot_title = ""){

  if(!all(c("site_no", "year", "value") %in% names(x))){
    stop("Not all required columns are provided")
  }
  
  if(is.na(num_years)){
    num_years <- diff(range(x$year))  
  }
  
  comp_data <- composite_data(x, 
                              num_years = num_years,
                              parameter_cd = parameter_cd)
  
  if(nrow(comp_data) == 0){
    stop("No sites had measurements for each of the years")
  }

  plot_out <- ggplot(data = comp_data) +
    geom_line(aes(x = year, y = value, color = name)) +
    hasp_framework(x_label = "Years", include_y_scale = FALSE,
                   y_label = "Principal Aquifer Water-Level Index",
                   plot_title = plot_title, zero_on_top = NA) +
    scale_y_reverse(sec.axis = dup_axis(labels =  NULL,
                                        name = NULL)) +
    scale_x_continuous(sec.axis = dup_axis(labels =  NULL,
                                           name = NULL)) +
    scale_color_manual(paste0("EXPLANATION\nComposite Annual\n",
                              attr(comp_data, "n_sites"), " sites"), 
                       values = c("red", "blue"), 
                       labels = levels(comp_data$name))
  
  return(plot_out)
}

#' Plot normalized composite hydrograph data
#'
#' Create composite hydrograph plot
#' 
#' @param x aquifer data frame. Requires at least 3 columns. Two are required "site_no", "year",
#' and "value".
#' @param num_years integer number of years required to the analysis. If \code{NA}, the 
#' analysis will default to the range of the data in x.
#' @param plot_title character title of plot.
#' @param parameter_cd character, 5-digit parameter code, default is "72019".
#' @return ggplot2 object
#' 
#' @import ggplot2
#' @export
#' @examples 
#' aquifer_data <- aquifer_data
#' 
#' norm_data <- plot_normalized_data(aquifer_data, 
#'                                   parameter_cd = "72019",
#'                                   num_years = 30)
#' norm_data
#' 
#' aquifer_data$cal_year <- aquifer_data$year
#' aquifer_data$year <- aquifer_data$water_year
#' plot_normalized_data(aquifer_data, 
#'                      parameter_cd = "72019",
#'                      num_years = num_years)
plot_normalized_data <- function(x, 
                                 num_years = NA,
                                 parameter_cd = "72019",
                                 plot_title = ""){
  
  if(!all(c("site_no", "year", "value") %in% names(x))){
    stop("Not all required columns are provided")
  }
  
  if(is.na(num_years)){
    num_years <- diff(range(x$year))  
  }
  
  norm_data <- normalized_data(x, 
                               num_years =  num_years,
                               parameter_cd = parameter_cd)
  
  plot_out <- ggplot(data = norm_data) +
    geom_line(aes(x = year, y = value, color = name)) +
    hasp_framework(x_label = "Years", include_y_scale = FALSE,
                   y_label = "Water-Level Index, feet below land surface",
                   plot_title = plot_title, zero_on_top = NA) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                    sec.axis = dup_axis(labels =  NULL,
                                        name = NULL)) +
    scale_x_continuous(sec.axis = dup_axis(labels =  NULL,
                                           name = NULL)) +
    scale_color_manual(paste0("EXPLANATION\nPercent Variation\n", 
                       attr(norm_data, "n_sites"), " sites"),
                       values = c("red", "blue"), 
                       labels = levels(norm_data$name))
  
  return(plot_out)
}


#' Map data used in composite hydrographs
#'
#' Map data used in composite hydrographs
#' 
#' @param x aquifer data frame. Requires at least 3 columns. Two are required "site_no", "year",
#' and "value".
#' @param num_years integer number of years required
#' @param parameter_cd character, 5-digit parameter code, default is "72019".
#' @return leaflet object
#' 
#' @export
#' @examples 
#' aquifer_data <- aquifer_data
#' num_years <- 30
#' 
#' map_data <- map_hydro_data(aquifer_data, num_years)
#' map_data
map_hydro_data <- function(x, num_years, parameter_cd = "72019"){
  
  x <- filter_sites(x, num_years, parameter_cd = parameter_cd)
  
  map_data <- prep_map_data(x)

  map <- leaflet::leaflet(data = map_data) %>%
    leaflet::addProviderTiles("CartoDB.Positron") %>%
    leaflet::addCircleMarkers(lat = ~dec_lat_va, lng = ~dec_long_va,
                               radius = 3,
                               fillOpacity = 1,
                               popup = ~popup,
                               stroke = FALSE) %>% 
    leaflet::fitBounds(~min(dec_long_va), ~min(dec_lat_va),
                       ~max(dec_long_va), ~max(dec_lat_va)) 
  
  return(map)
  
}