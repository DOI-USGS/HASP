#' Plot composite hydrograph data
#'
#' Create composite hydrograph plot
#' 
#' @param x aquifer data
#' @param sum_col column name
#' @param num_years integer number of years required
#' @return ggplot2 object
#' 
#' @import ggplot2
#' @export
#' @examples 
#' aquifer_data <- aquifer_data
#' sum_col <- "lev_va"
#' num_years <- 30
#' 
#' comp_data <- plot_composite_data(aquifer_data, sum_col, num_years)
#' comp_data
plot_composite_data <- function(x, sum_col, num_years){
  
  year <- value <- name <- ".dplyr"
  
  comp_data <- composite_data(x, sum_col, num_years)
  
  plot_out <- ggplot(data = comp_data) +
    geom_line(aes(x = year, y = value, color = name)) +
    theme_bw() +
    scale_y_reverse() +
    theme(legend.title = element_blank(),
          legend.position="bottom") +
    ylab("Principal Aquifer Water-Level Index") +
    xlab("Years")
  
  return(plot_out)
}

#' Plot normalized composite hydrograph data
#'
#' Create composite hydrograph plot
#' 
#' @param x aquifer data
#' @param sum_col column name
#' @param num_years integer number of years required
#' @return ggplot2 object
#' 
#' @import ggplot2
#' @export
#' @examples 
#' aquifer_data <- aquifer_data
#' sum_col <- "lev_va"
#' num_years <- 30
#' 
#' norm_data <- plot_normalized_data(aquifer_data, sum_col, num_years)
#' norm_data
plot_normalized_data <- function(x, sum_col, num_years){
  
  year <- value <- name <- ".dplyr"

  norm_data <- normalized_data(x, sum_col, num_years)
  
  plot_out <- ggplot(data = norm_data) +
    geom_line(aes(x = year, y = value, color = name)) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme(legend.title = element_blank(),
          legend.position="bottom") +
    ylab("Water-Level Index, feet below land surface") +
    xlab("Years")
  
  return(plot_out)
}


#' Map data used in composite hydrographs
#'
#' Map data used in composite hydrographs
#' 
#' @param x aquifer data
#' @param sum_col column name
#' @param num_years integer number of years required
#' @return leaflet object
#' 
#' @import leaflet
#' @export
#' @examples 
#' aquifer_data <- aquifer_data
#' sum_col <- "lev_va"
#' num_years <- 30
#' 
#' map_data <- map_hydro_data(aquifer_data, sum_col, num_years)
#' map_data
map_hydro_data <- function(x, sum_col, num_years){
  
  x <- filter_sites(x, sum_col, num_years)
  
  map_data <- prep_map_data(x, sum_col)

  map <- leaflet(data = map_data) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(lat = ~dec_lat_va, lng = ~dec_long_va,
                     radius = 3,
                     fillOpacity = 1,
                     popup= ~popup,
                     stroke=FALSE) %>% 
    fitBounds(~min(dec_long_va), ~min(dec_lat_va),
              ~max(dec_long_va), ~max(dec_lat_va)) 
  
  return(map)
  
}