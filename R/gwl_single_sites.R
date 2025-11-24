#' Single site groundwater level plots and tables
#' 
#' Function to create the field groundwater level data plot.
#' 
#' @export
#' @inheritParams monthly_frequency_table
#' @param plot_title character, title for plot.
#' @param subtitle character. Sub-title for plot, default is "U.S. Geological Survey".
#' @import ggplot2
#' @rdname gwl_plot_field
#' 
#' @examples
#' 
#' site <- "USGS-263819081585801"
#' # gwl_data <- dataRetrieval::read_waterdata_field_measurements(monitoring_location_id = site)
#' gwl_data <- L2701_example_data$Discrete
#' 
#' pcodes <- dataRetrieval::read_waterdata_parameter_codes(
#'             parameter_code = unique(gwl_data$parameter_code)
#'           )
#' 
#' gwl_plot_field(gwl_data, 
#'                plot_title = "L2701_example", 
#'                parameter_cd = "62610",
#'                flip = FALSE,
#'                y_label = pcodes$parameter_name[pcodes$parameter_code == "62610"])
#'                
#' gwl_plot_field(gwl_data, 
#'                plot_title = "L2701_example",
#'                y_label = pcodes$parameter_name[pcodes$parameter_code == "62611"], 
#'                parameter_cd = "62611",
#'                flip = FALSE)
#'                
#' gwl_plot_field(gwl_data,  
#'                plot_title = "L2701_example",
#'                y_label = pcodes$parameter_name[pcodes$parameter_code == "72019"], 
#'                parameter_cd = "72019",
#'                flip = TRUE)
#'                
gwl_plot_field <- function(gwl_data, 
                           plot_title = "",
                           parameter_cd = NA,
                           date_col = "time",
                           value_col = "value",
                           approved_col = "approval_status",
                           flip = TRUE, y_label = "",
                           subtitle = "U.S. Geological Survey"){
  
  plot_out <- gwl_plot_all(NULL, 
                           gwl_data, 
                           parameter_cd = parameter_cd,
                           date_col = c(NA, date_col),
                           value_col = c(NA, value_col), 
                           approved_col = c(NA, approved_col),
                           y_label = y_label,
                           plot_title = plot_title,
                           add_trend = FALSE,
                           subtitle = subtitle,
                           flip = flip)
  return(plot_out)
  
}



#' @rdname gwl_plot_field
#' @export
#' @param y_label character for y-axis label. Consider using 
#' \code{\link[dataRetrieval]{read_waterdata_parameter_codes}} for USGS parameter_name.
#' @param add_trend logical. Uses \code{trend_test}.
#' @param days_required_per_month integer. Number of days required per month. 
#' Default is 14. Only used if add_trend is \code{TRUE} using daily data.
#' @param n_years integer. This is the number of years to calculate the trend on.
#' Default is 10. This can be a vector of years.
#' @param POR_trend a logical indicating whether to include a trend test
#' for the full period of record. Default is \code{TRUE}.
#' @examples 
#' site <- "USGS-263819081585801"
#' p_code_dv <- "62610"
#' statCd <- "00001"
#' # gw_level_dv <- dataRetrieval::read_waterdata_daily(monitoring_location_id = site,
#' #                                                    parameter_code = p_code_dv,
#' #                                                    statistic_id = statCd)
#'                                                     
#' gw_level_dv <- L2701_example_data$Daily
#'
#' site_info <- dataRetrieval::read_waterdata_monitoring_location(monitoring_location_id = site)
#' 
#' plot_title <- site_info$monitoring_location_name
#' pcodes <- dataRetrieval::read_waterdata_parameter_codes(parameter_code = p_code_dv)
#' 
#' gwl_plot_all(gw_level_dv, 
#'              NULL, 
#'              plot_title = plot_title,
#'              y_label = pcodes$parameter_name,
#'              flip = TRUE) 
#' 
#' # gwl_data <- dataRetrieval::read_waterdata_field_measurements(monitoring_location_id = site)
#' gwl_data <- L2701_example_data$Discrete
#' 
#' gwl_plot_all(gw_level_dv, 
#'              gwl_data, 
#'              parameter_cd = "62610",
#'              plot_title = plot_title,
#'              y_label = pcodes$parameter_name,
#'              add_trend = FALSE,
#'              flip = FALSE)
#'
#' gwl_plot_all(gw_level_dv,
#'              gwl_data,
#'              parameter_cd = "62610",
#'              n_years = c(5, 20),
#'              POR_trend = TRUE,
#'              y_label = pcodes$parameter_name,
#'              plot_title = plot_title,
#'              add_trend = TRUE)
#'              
#' gwl_plot_all(NULL, 
#'              gwl_data, 
#'              parameter_cd = "62610",
#'              plot_title = plot_title, 
#'              y_label = pcodes$parameter_name)
#' 
#' gwl_plot_all(NULL, 
#'              gwl_data, 
#'              parameter_cd = "62610",
#'              plot_title = plot_title,
#'              y_label = pcodes$parameter_name,
#'              add_trend = TRUE)
#' 
gwl_plot_all <- function(gw_level_dv, 
                         gwl_data, 
                         parameter_cd = NA,
                         date_col = c("time", "time"),
                         value_col = c("value", "value"),
                         approved_col = c("approval_status", "approval_status"),
                         y_label = "",
                         subtitle = "U.S. Geological Survey",
                         plot_title = "",
                         add_trend = FALSE,
                         n_years = 10,
                         POR_trend = TRUE,
                         flip = FALSE,
                         days_required_per_month = 14){

  data_list <- set_up_data(gw_level_dv = gw_level_dv, 
                           gwl_data = gwl_data, 
                           parameter_cd = parameter_cd,
                           date_col = date_col,
                           value_col = value_col, 
                           approved_col = approved_col)
  
  gw_level_dv <- data_list$gw_level_dv
  gwl_data <- data_list$gwl_data
  
  linetype = c('solid', 'dashed')
  
  if(data_list$includes["dv"]){
    complete_df <- data.frame(Date = seq.Date(from = min(gw_level_dv$Date, na.rm = TRUE),
                                       to = max(gw_level_dv$Date, na.rm = TRUE), 
                                       by = "day"))

    gw_complete <- complete_df |> 
      dplyr::left_join(gw_level_dv, by = "Date") |> 
      dplyr::mutate(year = as.numeric(format(Date, "%Y")) + 
                              as.numeric(format(Date, "%j"))/365,
                     is_na_before = is.na(dplyr::lag(Value)),
                     is_na_after = is.na(dplyr::lead(Value)),
                     is_point = is_na_after & is_na_before & !is.na(Value),
                     is_complete = !is.na(Value) & !is_na_after & !is_na_before,
                     Approve = ifelse(grepl(pattern = "Approved",  
                                          x =  Approve), "Approved", "Provisional"))

    gw_complete[is.na(gw_complete$Value), "Approve"] <- "Approved"

    plot_out <- ggplot() +
      geom_path(data = gw_complete,
                aes(x = Date, color = Approve, y = Value)) 
    
    if(sum(gw_complete$is_point) > 0){
      plot_out <- plot_out +
        geom_point(data = dplyr::filter(gw_complete, is_point),
                 aes(x = Date, color = Approve, y = Value), size = 0.2) 
    }
    
    plot_out <- plot_out +
      scale_color_manual(ifelse(data_list$includes["both"],
                                "Daily Data", "EXPLANATION\nDaily Data"),
                         values = c("Approved" = "blue", "Provisional" = "red"), 
                         labels = c("Approved",
                                    "Provisional"))
  } else {
    plot_out <- ggplot()
    
  }
  
  if(data_list$includes["gwl"]){
    plot_out <- plot_out +
      geom_point(data = gwl_data,
                 aes(x = Date, y = Value, 
                     fill = Approve),
                 size = 1.5, shape = 21, color = "transparent") +
      scale_fill_manual("EXPLANATION\nWater-Level\nMeasurement",
                        values = c("Approved" = "navy", "Provisional" = "red"), 
                        labels = c("Approved",
                                   "Provisional"))
  } 
  
  plot_out <- plot_out +
    hasp_framework("Years", y_label,
                   plot_title = plot_title, 
                   subtitle = subtitle)  +
    scale_x_date(sec.axis = dup_axis(labels =  NULL,
                                     name = NULL))  
  
  if(add_trend){
    
    all_data <- gw_level_dv |>
      dplyr::bind_rows(gwl_data) |>
      dplyr::filter(grepl("Approved", Approve))
    
    gw_monthly <- monthly_mean(all_data,
                               date_col = "Date",
                               value_col = "Value")

    trend_results <- trend_test(gw_level_dv[grepl("Approved", gw_level_dv$Approve),],
                                gwl_data = gwl_data[grepl("Approved", gwl_data$Approve),],
                                date_col = c("Date", "Date"),
                                value_col = c("Value", "Value"), 
                                approved_col = c("Approve", "Approve"),
                                n_years = n_years,
                                POR_trend = POR_trend,
                                days_required_per_month = days_required_per_month)
    
    if(!all(trend_results$trend == "Not significant")){
      seg_df <- create_segs(trend_results,
                            gw_monthly, 
                            date_col = "mid_date",
                            value_col = "mean_va")
      
      plot_out <- plot_out +
        geom_segment(data = seg_df, color = "forestgreen", linewidth = 1,
                     aes(x = x1, xend = x2, 
                         y = y1, yend = y2,
                         group = years, linetype = years)) +
        scale_linetype_discrete("Trend") +
        guides(fill = guide_legend(order = 1),
               color = guide_legend(order = 2),
               linetype = guide_legend(order = 3))
    }
  } else {
    plot_out <- plot_out +
      guides(fill = guide_legend(order = 1),
             color = guide_legend(order = 2))
  }
  
  if(flip){
    plot_out <- plot_out +
      scale_y_continuous(trans = "reverse",
                         sec.axis = dup_axis(labels =  NULL,
                                             name = NULL))
  } else {
    
    plot_out <- plot_out +
      scale_y_continuous(sec.axis = dup_axis(labels =  NULL,
                                             name = NULL))
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

set_up_data <- function(gw_level_dv, 
                        gwl_data, 
                        parameter_cd,
                        date_col,
                        value_col, 
                        approved_col){
  
  includes_gwl <- !(is.null(gwl_data) ||
    all(is.na(gwl_data))||
      nrow(gwl_data) == 0 )
  
  includes_dv <- !(is.null(gw_level_dv) ||
    all(is.na(gw_level_dv)) ||
    nrow(gw_level_dv) == 0 )
  
  includes_both <- includes_gwl & includes_dv
  
  if(includes_gwl && nrow(gwl_data) != 0){
    
    checkmate::assert_data_frame(gwl_data)
    
    date_col_per <- date_col[2]
    value_col_per <- value_col[2]
    approved_col_per <- approved_col[2]
      
    date_col_per <- ifelse(is.na(date_col_per), "time", date_col_per)
    checkmate::assert_string(date_col_per)
    
    approved_col_per <- ifelse(is.na(approved_col_per), "approval_status", approved_col_per)
    checkmate::assert_string(approved_col_per)
    
    if(is.na(value_col_per)){
        value_col_per <- "value"
    }
    
    checkmate::assert_string(value_col_per)
    
    if(!is.na(parameter_cd) & length(parameter_cd) == 1){
      parameter_cd <- c(parameter_cd, parameter_cd)
    }
      
    gwl_data <- filter_pcode(gwl_data, parameter_cd[2])
    
    needed_cols <- c(date_col_per, value_col_per, approved_col_per)
    checkmate::assertNames(names(gwl_data), must.include = needed_cols)

    if( !inherits(gwl_data[[date_col_per]], c("Date", "POSIXt"))){
      incomplete <- which(nchar(gwl_data[[date_col_per]]) != 10)
      warning("Removed ", length(incomplete), " rows containing incomplete dates")

      gwl_data <- gwl_data[-incomplete, ]

    }
    
    gwl_data$Date <- as.Date(gwl_data[[date_col_per]])
    gwl_data$Value <- gwl_data[[value_col_per]]
    gwl_data$Approve <- gwl_data[[approved_col_per]]
    
    gwl_data$year <- as.numeric(format(gwl_data[["Date"]], "%Y")) + 
      as.numeric(format(gwl_data[["Date"]], "%j"))/365 
    
    if("geometry" %in% names(gwl_data)){
      gwl_data$geometry <- NULL
      attr(gwl_data, "class") <- "data.frame"
      attr(gwl_data, "sf_column") <- NULL      
    }
    
    gwl_data <- gwl_data[, c("year", "Date", "Value", "Approve")]

    
  } else {
    gwl_data <- data.frame(year = numeric(),
                           Date = as.Date(character()),
                           Value = numeric(),
                           Approve = character())

  }
  
  if(includes_dv && nrow(gw_level_dv) != 0){
    
    checkmate::assert_data_frame(gw_level_dv)
    
    date_col_dv <- ifelse(is.na(date_col[1]), "time", date_col[1])
    value_col_dv <- ifelse(is.na(value_col[1]), "value", value_col[1])
    approved_dv <- ifelse(is.na(approved_col[1]),
                          "approval_status",
                          approved_col[1])   
    
    needed_cols <- c(date_col_dv, value_col_dv, approved_dv)
    checkmate::assertNames(names(gw_level_dv), must.include = needed_cols)
    
    gw_level_dv <- filter_pcode(gw_level_dv, parameter_cd[1])
    
    if(!all(c(date_col_dv, value_col_dv, approved_dv) %in% names(gw_level_dv))){
      stop("gw_level_dv data frame doesn't include all specified columns")
    }
    
    if( !inherits(gw_level_dv[[date_col_dv]], c("Date", "POSIXt"))){
      incomplete <- which(nchar(gw_level_dv[[date_col_dv]]) != 10)
      warning("Removed ", length(incomplete), " rows containing incomplete dates")
      
      gw_level_dv <- gw_level_dv[-incomplete, ]
      
    }
    
    #Convert date column to date just in case its a POSIXct:
    gw_level_dv$Date <- as.Date(gw_level_dv[[date_col_dv]])
    
    gw_level_dv$Value <- as.numeric(gw_level_dv[[value_col_dv]])
    gw_level_dv$Approve <- gw_level_dv[[approved_dv]]

    if("geometry" %in% names(gw_level_dv)){
      gw_level_dv$geometry <- NULL
      attr(gw_level_dv, "class") <- "data.frame"
      attr(gw_level_dv, "sf_column") <- NULL      
    }
    
    gw_level_dv <- gw_level_dv[, c("Date", "Value", "Approve")]
    
  } else {
    gw_level_dv <- data.frame(Date =  as.Date(character()),
                              Value = numeric(),
                              Approve = character())
  }
  
  includes <- c(includes_gwl, includes_dv, includes_both)
  names(includes) <- c("gwl", "dv", "both")
  
  return(list(gw_level_dv = gw_level_dv,
              gwl_data = gwl_data,
              includes = includes))
}

filter_pcode <- function(df, pcode){
  
  if("parameter_code" %in% names(df)){
    if(!all(is.na(pcode))){
      pcode <- dataRetrieval::zeroPad(pcode, 5)
      df <- df[!is.na(df$parameter_code) & 
                 df$parameter_code %in% pcode, ]
    } else if(all(is.na(pcode)) &
              length(unique(df$parameter_code)) > 1){
      warning("Multiple parameter codes detected in column 'parameter_code',
            and a parameter code is not specified in 'parameter_code'")
    }
  } else {
    if(!all(is.na(pcode))){
      message("gwl_data data frame does not contain a 'parameter_code' column,
            yet 'parameter_cd' is defined. Ignoring 'parameter_code' argument.")
    }
  }
  return(df)
}

