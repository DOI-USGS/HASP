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
#' plot_title <- attr(gwl_data, "siteInfo")[["station_nm"]]
#' gwl_plot_periodic(gwl_data, plot_title)
gwl_plot_periodic <- function(gwl_data, plot_title = "",
                              date_col = "lev_dt",
                              value_col = "sl_lev_va",
                              approved_col = "lev_age_cd"){
  
  if(!all(c(date_col, value_col, approved_col, "sl_datum_cd") %in% names(gwl_data))){
    stop("data frame gwl_data doesn't include all mandatory columns")
  }
  
  year <- ".dplyr"

  datum <- unique(gwl_data$sl_datum_cd)
  y_label <- sprintf("Elevation above %s, feet", datum)
  
  gwl_data$year <- as.numeric(format(gwl_data[[date_col]], "%Y")) + 
    as.numeric(as.character(gwl_data[[date_col]], "%j"))/365
  
  on_top <- zero_on_top(gwl_data[[value_col]])
  
  plot_out <- ggplot(data = gwl_data,
         aes_string(x = "year", y = value_col)) +
    geom_line(linetype = "dashed", color = "blue") +
    geom_point(aes_string(color = approved_col), size = 1) +
    hasp_framework("Years", y_label, plot_title, zero_on_top = on_top) +
    scale_color_manual("EXPLANATION\nWater-level\nmeasurement",
                       values = c("A" = "blue", "P" = "red"), 
                       labels = c("A" = "Approved",
                                  "P" = "Provisional")) +
    scale_x_continuous(sec.axis = dup_axis(labels =  NULL,
                                           name = NULL)) 

  return(plot_out)
  
}


#' @rdname gwl_periodic
#' @export
#' @importFrom rlang `:=`
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
#' plot_title <- attr(gwl_data, "siteInfo")[["station_nm"]]
#' gwl_plot_all(gw_level_dv, gwl_data, plot_title, p_code_dv = parameterCd)
#' 
#' gwl_plot_all(gw_level_dv, gwl_data, plot_title, add_trend = TRUE,
#'              p_code_dv = parameterCd)
#' 
#' gwl_plot_all(NULL, gwl_data, plot_title, p_code_dv = parameterCd)
gwl_plot_all <- function(gw_level_dv, 
                         gwl_data, 
                         plot_title = "",
                         date_col = "lev_dt",
                         value_col = "sl_lev_va",
                         approved_col = "lev_age_cd",
                         p_code_dv = "62610",
                         add_trend = FALSE){
  
  if(!all(c(date_col, value_col, approved_col) %in% names(gwl_data))){
    stop("data frame gwl_data doesn't include all mandatory columns")
  }
  
  x1 <- x2 <- y1 <- y2 <- trend <- year <- ".dplyr"
  Date <- is_na_after <- is_na_before <- is_point <- ".dplyr"
  
  datum <- unique(gwl_data$sl_datum_cd)
  y_label <- sprintf("Elevation above %s, feet", datum)
  linetype = c('solid', 'dashed')
  
  gwl_data$year <- as.numeric(format(gwl_data[[date_col]], "%Y")) + 
    as.numeric(as.character(gwl_data[[date_col]], "%j"))/365
  
  if(!all(is.null(gw_level_dv))){
    
    if(!all(c("Date") %in% names(gw_level_dv))){
      stop("data frame gw_level_dv doesn't include all mandatory columns")
    }
    
    complete_df <- data.frame(Date = seq.Date(from = min(gw_level_dv$Date, na.rm = TRUE),
                                       to = max(gw_level_dv$Date, na.rm = TRUE), 
                                       by = "day"))
    
    val_cols <- grep(p_code_dv, names(gw_level_dv))
    remark_col <- grep("_cd", names(gw_level_dv))
    remark_col <- remark_col[remark_col %in% val_cols]
    
    val_cols <- val_cols[!val_cols %in% remark_col]
    val_cols <- names(gw_level_dv)[val_cols]
    remark_col <- names(gw_level_dv)[remark_col]
    
    on_top <- zero_on_top(c(gw_level_dv[[val_cols]],
                            gwl_data[[value_col]]))

    gw_complete <- complete_df %>% 
      left_join(select(gw_level_dv, Date, !!val_cols, !!remark_col), by = "Date") %>% 
      mutate(year = as.numeric(format(Date, "%Y")) + 
               as.numeric(as.character(Date, "%j"))/365,
             is_na_before = is.na(lag(!!sym(val_cols))),
             is_na_after = is.na(lead(!!sym(val_cols))),
             is_point = is_na_after & is_na_before & !is.na(!!sym(val_cols)),
             is_complete = !is.na(!!sym(val_cols)) & !is_na_after & !is_na_before,
             !!remark_col := ifelse(grepl(pattern = "A",  
                                          x =  !!sym(remark_col)), "A", "P"))

    gw_complete[is.na(gw_complete[[val_cols]]), remark_col] <- "A"

    plot_out <- ggplot() +
      geom_path(data = gw_complete,
                aes_string(x = "year", color = remark_col,
                           y = val_cols)) 
    
    if(sum(gw_complete$is_point) > 0){
      plot_out <- plot_out +
        geom_point(data = filter(gw_complete, is_point),
                 aes_string(x = "year", color = remark_col,
                            y = val_cols), size = 0.2) 
    }
    
    plot_out <- plot_out +
      scale_color_manual("Daily Data",
                         values = c("A" = "blue", "P" = "red"), 
                         labels = c("A" = "Approved",
                                    "P" = "Provisional"))
  } else {
    plot_out <- ggplot()
    on_top <- zero_on_top(gwl_data[[value_col]])

  }
  
  plot_out <- plot_out +
    geom_point(data = gwl_data,
               aes_string(x = "year", 
                          y = value_col, 
                          fill = approved_col),
               size = 1.5, shape = 21, color = "transparent") +
    hasp_framework("Years", y_label, plot_title, zero_on_top = on_top) +
    theme(aspect.ratio = NULL) +
    scale_fill_manual("EXPLANATION\nWater-Level\nMeasurement",
                       values = c("A" = "navy", "P" = "red"), 
                       labels = c("A" = "Approved",
                                  "P" = "Provisional")) +
    scale_x_continuous(sec.axis = dup_axis(labels =  NULL,
                                           name = NULL)) 
  
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

zero_on_top <- function(x){
  on_top <- max(range(x), na.rm = TRUE) < 0
  if(!on_top){
    if(min(range(x), na.rm = TRUE) < 0){
      on_top <- NA
    }
  }
  return(on_top)
}

