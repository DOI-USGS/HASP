#' Single site groundwater level plots and tables
#' 
#' Funtion to create the periodic groundwater data plot.
#' @export
#' @param x data frame returned from dataRetrieval::readNWISgwl
#' @param title character
#' @import ggplot2
#' @import dplyr
#' @rdname gwl_periodic
#' 
#' @examples
#' 
#' site <- "263819081585801"
#' gwl_data <- dataRetrieval::readNWISgwl(site)
#' gwl_plot_periodic(gwl_data)
gwl_plot_periodic <- function(x, title = ""){
  
  if(!all(c("lev_dateTime", "sl_lev_va", "lev_age_cd") %in% names(x))){
    stop("data frame x doesn't include all mandatory columns")
  }
  
  lev_dateTime <- sl_lev_va <- ".dplyr"
  datum <- unique(x$sl_datum_cd)
  y_label <- sprintf("Elevation above %s, feet", datum)
  
  plot_out <- ggplot(data = x,
         aes(x = lev_dateTime, y = sl_lev_va)) +
    geom_line(linetype = "dashed", color = "blue") +
    geom_point(aes(color = lev_age_cd), size = 1) +
    theme_gwl() +
    labs(caption = paste("Plot created:", Sys.Date()), 
         y = y_label, x = "Years") +
    expand_limits(y = 0) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0))) +
    scale_color_manual("",
                       values = c("A" = "blue", "P" = "red"), 
                       labels = c("A" = "Approved water-level measurement",
                                  "P" = "Provisional water-level measurement")) +
    ggtitle(title, 
            subtitle = "U.S. Geological Survey") +
    theme(legend.position = "bottom",
          legend.direction = "vertical",
          legend.title = element_blank())

  return(plot_out)
  
}

#' @rdname gwl_periodic
#' @export
#' @examples 
#' site <- "263819081585801"
#' gw_gwl_data <- dataRetrieval::readNWISgwl(site)
gwl_summary_periodic <- function(x){
  
}

#' parameterCd <- "62610"
#' statCd <- "00001"
#' gw_dv_data <- dataRetrieval::readNWISdv(site, parameterCd, statCd = statCd)
#' 
#' gw_uv_data <- dataRetrieval::readNWISuv(site, parameterCd)
#' 
#' gw_qw_data  <- dataRetrieval::readNWISqw(site, parameterCd)
