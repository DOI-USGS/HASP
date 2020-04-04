#' Single site groundwater level plots and tables
#' 
#' Funtion to create the periodic groundwater data plot.
#' @export
#' @param gwl_data data frame returned from dataRetrieval::readNWISgwl
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
gwl_plot_periodic <- function(gwl_data, plot_title = "",
                              date_col = "lev_dt",
                              value_col = "sl_lev_va",
                              approved_col = "lev_age_cd"){
  
  if(!all(c(date_col, value_col, approved_col, "sl_datum_cd") %in% names(gwl_data))){
    stop("data frame gwl_data doesn't include all mandatory columns")
  }

  datum <- unique(gwl_data$sl_datum_cd)
  y_label <- sprintf("Elevation above %s, feet", datum)
  
  plot_out <- ggplot(data = gwl_data,
         aes_string(x = date_col, y = value_col)) +
    geom_line(linetype = "dashed", color = "blue") +
    geom_point(aes_string(color = approved_col), size = 1) +
    hasp_framework("Years", y_label, plot_title, zero_on_top = TRUE) +
    scale_color_manual("EXPLANATION\nWater-level measurement",
                       values = c("A" = "blue", "P" = "red"), 
                       labels = c("A" = "Approved",
                                  "P" = "Provisional")) +
    theme(legend.direction = "vertical")

  return(plot_out)
  
}


#' @rdname gwl_periodic
#' @export
#' @param gw_level_dv daily value groundwater levels. Must include columns 
#' @param p_code_dv daily parameter code. Default is "62610".
#' @param add_trend logical. Uses \code{kendell_test_5_20_years}.
#' @examples 
#' # site <- "263819081585801"
#' parameterCd <- "62610"
#' # statCd <- "00001"
#' # gw_level_dv <- dataRetrieval::readNWISdv(site, parameterCd, statCd = statCd)
#' # Using package example data:
#' gw_level_dv <- L2701_example_data$Daily
#' gwl_plot_all(gw_level_dv, gwl_data, p_code_dv = parameterCd)
#' 
#' gwl_plot_all(gw_level_dv, gwl_data, add_trend = TRUE,
#'              p_code_dv = parameterCd)
#' 
#' gwl_plot_all(NULL, gwl_data, p_code_dv = parameterCd)
gwl_plot_all <- function(gw_level_dv, gwl_data, 
                         plot_title = "",
                         date_col = "lev_dt",
                         value_col = "sl_lev_va",
                         approved_col = "lev_age_cd",
                         p_code_dv = "62610",
                         add_trend = FALSE){
  
  if(!all(c(date_col, value_col, approved_col) %in% names(gwl_data))){
    stop("data frame gwl_data doesn't include all mandatory columns")
  }
  
  x1 <- x2 <- y1 <- y2 <- trend <- ".dplyr"
  
  datum <- unique(gwl_data$sl_datum_cd)
  y_label <- sprintf("Elevation above %s, feet", datum)
  linetype = c('solid', 'dashed')
  
  plot_out <- ggplot() +
    geom_point(data = gwl_data,
               aes_string(x = date_col, y = value_col, fill = approved_col),
               size = 1.5, shape = 21, color = "transparent") 
    
  if(!all(is.null(gw_level_dv))){
    
    if(!all(c("Date") %in% names(gw_level_dv))){
      stop("data frame gw_level_dv doesn't include all mandatory columns")
    }
    
    val_cols <- grep(p_code_dv, names(gw_level_dv))
    remark_col <- grep("_cd", names(gw_level_dv))
    remark_col <- remark_col[remark_col %in% val_cols]
    
    val_cols <- val_cols[!val_cols %in% remark_col]
    val_cols <- names(gw_level_dv)[val_cols]
    remark_col <- names(gw_level_dv)[remark_col]
    
    plot_out <- plot_out +
      geom_line(data = gw_level_dv,
                aes_string(x = "Date", y = val_cols, color = remark_col),
                linetype = "dashed") +
      scale_color_manual("Daily Data",
                         values = c("A" = "blue", "P" = "red"), 
                         labels = c("A" = "Approved",
                                    "P" = "Provisional"))
  } 
  
  plot_out <- plot_out +
    hasp_framework("Years", y_label, plot_title, zero_on_top = TRUE) +
    scale_fill_manual("EXPLANATION\nWater-Level\nMeasurement",
                       values = c("A" = "blue", "P" = "red"), 
                       labels = c("A" = "Approved",
                                  "P" = "Provisional")) 
  
  if(add_trend){
    
    gw_monthly <- monthly_mean(gw_level_dv, 
                               date_col = "Date", 
                               value_col = val_cols)
    
    seg_df <- create_segs(gw_monthly, 
                          date_col = "mid_date",
                          value_col = "mean_va")
    
    plot_out <- plot_out +
      geom_segment(data = seg_df, color = "forestgreen", size = 1,
                   aes(x = x1, xend = x2, 
                       y = y1, yend = y2,
                       group = trend, linetype = trend)) +
      scale_linetype_manual("Trend", 
                            values = linetype,
                            breaks = c("5-year trend", "20-year trend"),
                            labels = c("5 year", "20 year")) +
      guides(fill = guide_legend(order = 1),
             color = guide_legend(order = 2),
             linetype = guide_legend(order = 3))
  } else {
    plot_out <- plot_out +
      guides(fill = guide_legend(order = 1),
             color = guide_legend(order = 2))
  }
  
  return(plot_out)
  
}

 

