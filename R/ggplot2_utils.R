
#' ggplot2 themes and accesories for package
#' 
#' Themes and other useful graphing utilities for the HASP package
#' 
#' @export
#' @rdname themes
#' @param base_family character base font family
#' @param \dots additional arguments
#' @examples 
#' 
#' ggplot2::ggplot() + theme_gwl()
#' 
#' 
theme_gwl <- function(base_family = "", ...){
  
  spacing_1 <- c(0.03)
  spacing_2 <- c(-0.01)
  
  theme_bw(base_family = base_family, ...) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 9),
      axis.ticks.length = unit(-0.05, "in"),
      plot.subtitle = element_text(color = "darkgreen", size = 8),
      axis.text.x = element_text(margin = margin(t = spacing_1, r = 0, b = 0, l = 0, unit = "cm")),
      axis.title.x = element_text(margin = margin(t = spacing_2, r = 0, b = 0, l = 0, unit = "cm")),
      axis.text.y = element_text(margin = margin(t = 0, r = spacing_1, b = 0, l = 0, unit = "cm")),
      axis.title.y = element_text(margin = margin(t = 0, r = spacing_2, b = 0, l = 0, unit = "cm")), 
      plot.caption = element_text(hjust = 1, size = 8),
      legend.direction = "vertical",
      legend.box = "horoizontal",
      legend.spacing.y = unit(0.15, "cm"),
      legend.margin = margin(unit(0, units = "cm")),
      legend.box.margin = margin(unit(0, units = "cm"))
    )
}

#' @export
#' @rdname themes
#' @param x_label character. Label for x-axis.
#' @param y_label character. Label for y-axis.
#' @param include_y_scale logical. If \code{TRUE}, include groundwater type style for y-axis. Default is \code{FALSE}.
#' @param plot_title character. Title for plot.
#' @param subtitle character. Sub-title for plot, default is "U.S. Geological Survey".
#' @param zero_on_top logical. If zero_on_top is \code{TRUE}, there is no padding
#' at the top of y axis. If \code{FALSE}, no padding at the bottom. If \code{NA}, padding on both top and bottom.
#' @examples 
#' 
#' x_label <- "Date"
#' y_label <- "Level"
#' plot_title <- "Super Site"
#' 
#' ggplot2::ggplot() + 
#'      hasp_framework(x_label, 
#'                     y_label, 
#'                     plot_title)
hasp_framework <- function(x_label, y_label,
                           plot_title, 
                           include_y_scale = FALSE, 
                           zero_on_top = TRUE,
                           subtitle = "U.S. Geological Survey"){
  
  if(is.na(zero_on_top)) {
    expand_lims <- c(0.05, 0.05)
    yrange <- c(NA, NA)
  } else if (zero_on_top){
    expand_lims <- c(0.05, 0)
    yrange <- c(NA, 0)
  } else {
    expand_lims <- c(0, 0.05)
    yrange <- c(0, NA)
  }
  
  if(include_y_scale){
    basic_elements <- list(theme_gwl(),
                           labs(caption = paste("Plot created:", Sys.Date()), 
                                y = y_label, x = x_label),
                           expand_limits(y = 0),
                           coord_cartesian(ylim = yrange),
                           scale_y_continuous(expand = expansion(mult = expand_lims),
                                              sec.axis = dup_axis(labels =  NULL,
                                                                  name = NULL)),
                           ggtitle(plot_title, 
                                   subtitle = subtitle))
  } else {
    basic_elements <- list(theme_gwl(),
                           labs(caption = paste("Plot created:", Sys.Date()), 
                                y = y_label, x = x_label),
                           ggtitle(plot_title, 
                                   subtitle = subtitle))
  }
  
  return(basic_elements)
}

