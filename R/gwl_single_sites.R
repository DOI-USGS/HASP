#' Single site groundwater level plots and tables
#' 
#' Funtion to create the periodic groundwater data plot.
#' @export
#' @param gwl data frame returned from dataRetrieval::readNWISgwl
#' @param plot_title character
#' @param date_col name of date column. Default is "lev_dt".
#' @param value_col name of value column. Default is "sl_lev_va".
#' @param approved_col name of column to get provisional/approved status.
#' Default is "lev_age_cd".
#' @import ggplot2
#' @import dplyr
#' @rdname gwl_periodic
#' 
#' @examples
#' 
#' # site <- "263819081585801"
#' # gwl_data <- dataRetrieval::readNWISgwl(site)
#' 
#' # Using package example data:
#' gwl_data <- L2701_example_data$Discrete
#' title <- attr(gwl_data, "siteInfo")[["station_nm"]]
#' gwl_plot_periodic(gwl_data, plot_title = title)
gwl_plot_periodic <- function(gwl, plot_title = "",
                              date_col = "lev_dt",
                              value_col = "sl_lev_va",
                              approved_col = "lev_age_cd"){
  
  if(!all(c(date_col, value_col, approved_col, "sl_datum_cd") %in% names(gwl))){
    stop("data frame gwl doesn't include all mandatory columns")
  }

  datum <- unique(gwl$sl_datum_cd)
  y_label <- sprintf("Elevation above %s, feet", datum)
  
  plot_out <- ggplot(data = gwl,
         aes_string(x = date_col, y = value_col)) +
    geom_line(linetype = "dashed", color = "blue") +
    geom_point(aes_string(color = approved_col), size = 1) +
    theme_gwl() +
    labs(caption = paste("Plot created:", Sys.Date()), 
         y = y_label, x = "Years") +
    expand_limits(y = 0) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0))) +
    scale_color_manual("",
                       values = c("A" = "blue", "P" = "red"), 
                       labels = c("A" = "Approved water-level measurement",
                                  "P" = "Provisional water-level measurement")) +
    ggtitle(plot_title, 
            subtitle = "U.S. Geological Survey") +
    theme(legend.position = "bottom",
          legend.direction = "vertical",
          legend.title = element_blank())

  return(plot_out)
  
}


#' @rdname gwl_periodic
#' @export
#' @param dv daily value groundwater levels. Must include columns 
#' @param add_trend logical. Uses \code{kendell_test_5_20_years}.
#' @examples 
#' # site <- "263819081585801"
#' # parameterCd <- "62610"
#' # statCd <- "00001"
#' # dv <- dataRetrieval::readNWISdv(site, parameterCd, statCd = statCd)
#' # Using package example data:
#' dv <- L2701_example_data$Daily
#' gwl_plot_all(dv, gwl_data)
gwl_plot_all <- function(dv, gwl, 
                         plot_title = "",
                         add_trend = FALSE){
  
  if(!all(c("lev_dt", "sl_lev_va", "lev_age_cd") %in% names(gwl))){
    stop("data frame gwl doesn't include all mandatory columns")
  }

  if(!all(c("Date") %in% names(dv))){
    stop("data frame dv doesn't include all mandatory columns")
  }
  
  val_cols <- grep("62610", names(dv))
  remark_col <- grep("_cd", names(dv))
  remark_col <- remark_col[remark_col %in% val_cols]
  
  val_cols <- val_cols[!val_cols %in% remark_col]
  val_cols <- names(dv)[val_cols]
  remark_col <- names(dv)[remark_col]
  
  # seg_df <- create_segs(qw_sub)
  
  lev_dt <- sl_lev_va <- lev_age_cd <- ".dplyr"
  datum <- unique(gwl$sl_datum_cd)
  y_label <- sprintf("Elevation above %s, feet", datum)
  
  plot_out <- ggplot() +
    geom_line(data = dv,
              aes_string(x = "Date", y = val_cols, color = remark_col),
              linetype = "dashed") +
    geom_point(data = gwl,
               aes(x = lev_dt, y = sl_lev_va, fill = lev_age_cd),
               size = 1.5, shape = 21, color = "transparent") +
    theme_gwl() +
    labs(caption = paste("Plot created:", Sys.Date()), 
         y = y_label, x = "Years") +
    expand_limits(y = 0) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0))) +
    scale_color_manual("Daily Data",
                       values = c("A" = "blue", "P" = "red"), 
                       labels = c("A" = "Approved",
                                  "P" = "Provisional")) +
    scale_fill_manual("Water-Level Measurement",
                       values = c("A" = "blue", "P" = "red"), 
                       labels = c("A" = "Approved",
                                  "P" = "Provisional")) +
    ggtitle(plot_title, 
            subtitle = "U.S. Geological Survey") +
    theme(legend.position = "bottom",
          legend.direction = "vertical")
  
  return(plot_out)
  
}

 

