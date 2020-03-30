
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
  theme_bw(base_family = base_family, ...) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5),
      axis.ticks.length = unit(-0.05, "in"),
      plot.subtitle = element_text(color = "darkgreen"),
      axis.text.y = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm")),
      axis.text.x = element_text(margin=unit(c(0.3,0.3,0.3,0.3), "cm")),
      aspect.ratio = 1,
      plot.caption = element_text(hjust = 1)
    )
}
