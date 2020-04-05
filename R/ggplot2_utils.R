
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
  
  spacing_1 <- c(0.3, 0.3, 0.3, 0.3)
  spacing_2 <- c(-0.1, -0.1, -0.1, -0.10)
  
  theme_bw(base_family = base_family, ...) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 9),
      axis.ticks.length = unit(-0.05, "in"),
      plot.subtitle = element_text(color = "darkgreen", size = 8),
      axis.text.x = element_text(margin=unit(spacing_1, units = "cm")),
      axis.title.x = element_text(margin=unit(spacing_2, units = "cm")),
      axis.text.y = element_text(margin=unit(spacing_1, units = "cm")),
      axis.title.y = element_text(margin=unit(spacing_2, units = "cm")),
      aspect.ratio = 1,
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
#' @param x_label character
#' @param y_label character
#' @param plot_title character
#' @param zero_on_top logical
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
                           plot_title, zero_on_top = TRUE){
  
  if(is.na(zero_on_top)) {
    expand_lims <- c(0.05, 0.05)
  } else if (zero_on_top){
    expand_lims <- c(0.05, 0)
  } else {
    expand_lims <- c(0, 0.05)
  }
  
  basic_elements <- list(theme_gwl(),
                         labs(caption = paste("Plot created:", Sys.Date()), 
                              y = y_label, x = x_label),
                         expand_limits(y = 0),
                         scale_y_continuous(expand = expansion(mult = expand_lims),
                                            sec.axis = dup_axis(labels =  NULL,
                                                                name = NULL)),
                         ggtitle(plot_title, 
                                 subtitle = "U.S. Geological Survey"))
  
  
}

