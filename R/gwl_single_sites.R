#' Single site groundwater level plots and tables
#' 
#' Function to create the field groundwater level data plot.
#' @export
#' @param gwl_data data frame returned from dataRetrieval::readNWISgwl, or 
#' data frame with mandatory columns lev_dt (representing date), lev_age_cd (representing
#' approval code), and a column representing the measured value (either lev_va,
#' sl_lev_va, or value).
#' @param value_col name of value column. If NA, the code will attempt to autogenerate
#' based on parameter_cd
#' @param date_col name of date column. Default is "lev_dt".
#' @param approved_col name of approval column. Default is "lev_age_cd".
#' @param plot_title character
#' @param parameter_cd Parameter code to be filtered to in a column specifically
#' named "parameter_cd". If the data doesn't come directly from NWIS services, this 
#' can be set to \code{NA},and this argument will be ignored.
#' @param flip_y logical. If \code{TRUE}, flips the y axis so that the smallest number is on top.
#' Default is \code{TRUE}.
#' @import ggplot2
#' @rdname gwl_plot_field
#' 
#' @examples
#' 
#' # site <- "263819081585801"
#' # gwl_data <- dataRetrieval::readNWISgwl(site)
#' 
#' # Using package example data:
#' gwl_data <- L2701_example_data$Discrete
#' plot_title <- attr(gwl_data, "siteInfo")[["station_nm"]]
#' pcodes <- dataRetrieval::readNWISpCode(unique(gwl_data$parameter_cd))
#' gwl_plot_field(gwl_data, paste(plot_title,
#'                          pcodes$parameter_nm[pcodes$parameter_cd == "62610"]), 
#'                parameter_cd = "62610",
#'                flip_y = FALSE)
#' gwl_plot_field(gwl_data,  paste(plot_title,
#'                pcodes$parameter_nm[pcodes$parameter_cd == "62611"]), 
#'                parameter_cd = "62611",
#'                flip_y = FALSE)
#' gwl_plot_field(gwl_data,  paste(plot_title,
#'                          pcodes$parameter_nm[pcodes$parameter_cd == "72019"]), 
#'                parameter_cd = "72019")
gwl_plot_field <- function(gwl_data, plot_title = "",
                           parameter_cd = NA,
                           date_col = "lev_dt",
                           value_col = NA,
                           approved_col = "lev_age_cd",
                           flip_y = TRUE, y_label = ""){
  
  value_col <- get_value_column(parameter_cd, gwl_data, value_col )

  if(!all(c(date_col, value_col, approved_col) %in% names(gwl_data))){
    missing_cols <- c(date_col, value_col, approved_col)[!c(date_col,
                                                           value_col,
                                                           approved_col) %in%
                                                      names(gwl_data)]
    stop("data frame gwl_data doesn't include mandatory column(s):", 
         paste(missing_cols, collapse = ", "))
  }
  
  gwl_data <- filter_pcode(gwl_data, parameter_cd)

  if(!is.na(parameter_cd)){
    y_label <- dataRetrieval::readNWISpCode(parameter_cd)[["parameter_nm"]]
  } 
  
  
  gwl_data$year <- as.numeric(format(gwl_data[[date_col]], "%Y")) + 
    as.numeric(as.character(gwl_data[[date_col]], "%j"))/365
  
  plot_out <- ggplot(data = gwl_data,
         aes_string(x = "year", y = value_col)) +
    geom_line(linetype = "dashed", color = "blue") +
    geom_point(aes_string(color = approved_col), size = 1) +
    hasp_framework("Years", y_label, plot_title = plot_title) +
    scale_color_manual("EXPLANATION\nWater-level\nmeasurement",
                       values = c("A" = "blue", "P" = "red"), 
                       labels = c("A" = "Approved",
                                  "P" = "Provisional")) +
    scale_x_continuous(sec.axis = dup_axis(labels =  NULL,
                                           name = NULL)) 

  if(flip_y){
    plot_out <- plot_out +
      scale_y_continuous(trans = "reverse")
  }
  return(plot_out)
  
}


get_value_column <- function(parameter_cd, df, value_col){
  
  if(is.na(parameter_cd)){
    if("parameter_cd" %in% names(df)){
      parameter_cd <- unique(df$parameter_cd)[1]
      message("parameter_cd unspecified, using", parameter_cd)
    }
  }
  
  if(!is.na(parameter_cd) & any(grepl(parameter_cd, names(df)))){
    
    value_col_df <- names(df)[grepl(parameter_cd, names(df))]
    value_col_df <- unique(gsub("_cd", "", value_col_df))
    
    if(!is.na(value_col) & !is.na(parameter_cd)){
      if(!all(value_col == value_col_df)){
        if(value_col %in% names(df)){
          message("parameter_cd provided does not match value_col, using:", value_col)
        }
      }
    } else {
      value_col <- value_col_df
    }
    return(value_col)
  }
  
  if(!is.na(parameter_cd)){
    if(parameter_cd == "72019"){
      value_col <- "lev_va"
    } else if(is.na(value_col)){
      value_col <- "sl_lev_va"
    } 
  } 
  
  return(value_col)
}

#' @rdname gwl_plot_field
#' @export
#' @param y_label character for y-axis label. Consider using \code{\link[dataRetrieval]{readNWISpCode}} for USGS parameter_nm.
#' @param add_trend logical. Uses \code{kendell_test_5_20_years}.
#' @param gw_level_dv daily value groundwater levels. Must include columns specified in date_col, value_col, and approved_col.
#' @examples 
#' # site <- "263819081585801"
#' parameterCd <- "62610"
#' # statCd <- "00001"
#' # gw_level_dv <- dataRetrieval::readNWISdv(site, parameterCd, statCd = statCd)
#' # Using package example data:
#' gw_level_dv <- L2701_example_data$Daily
#' gwl_data <- L2701_example_data$Discrete
#' plot_title <- attr(gwl_data, "siteInfo")[["station_nm"]]
#' pcodes <- dataRetrieval::readNWISpCode(unique(gwl_data$parameter_cd))
#' 
#' gwl_plot_all(gw_level_dv, 
#'              NULL, 
#'              parameter_cd = "62610",
#'              plot_title = plot_title,
#'              flip_y = FALSE) 
#' 
#' gwl_plot_all(gw_level_dv, 
#'              gwl_data, 
#'              parameter_cd = "62610",
#'              plot_title = paste(plot_title,
#'                          pcodes$parameter_nm[pcodes$parameter_cd == "62610"]),
#'              add_trend = TRUE)
#'              
#' gwl_plot_all(NULL, 
#'              gwl_data, 
#'              parameter_cd = "62610",
#'              plot_title = paste(plot_title,
#'                          pcodes$parameter_nm[pcodes$parameter_cd == "62610"]))
#' 
gwl_plot_all <- function(gw_level_dv, 
                         gwl_data, 
                         parameter_cd = NA,
                         date_col = NA,
                         value_col = NA, approved_col = NA,
                         y_label = "GWL",
                         plot_title = "",
                         add_trend = FALSE,
                         flip_y = FALSE){

  includes_gwl <- !is.null(gwl_data) || !all(is.na(gwl_data))
  includes_dv <- !is.null(gw_level_dv) || !all(is.na(gw_level_dv))
  includes_both <- includes_gwl & includes_dv
  
  if(includes_gwl){
    
    if(includes_both){
      date_col_per <- date_col[2]
      value_col_per <- value_col[2]
      approved_col_per <- approved_col[2]
      
    } else {
      date_col_per <- date_col[1]
      value_col_per <- value_col[1]
      approved_col_per <- approved_col[1]
      
    }
    date_col_per <- ifelse(is.na(date_col_per), "lev_dt", date_col_per)
    approved_col_per <- ifelse(is.na(approved_col_per), "lev_age_cd", approved_col_per)
    value_col_per <- get_value_column(parameter_cd, gwl_data, value_col_per)
      
    gwl_data <- filter_pcode(gwl_data, parameter_cd)
    
    gwl_data[, value_col_per] <- as.numeric(gwl_data[[value_col_per]])
    
    if(all(is.na(gwl_data[[value_col_per]]))){
      value_col_init <- value_col_per
      if(value_col_per == "lev_va") {
        value_col_per <- "sl_lev_va"
      } else if (value_col_per == "sl_lev_va"){
        value_col_per <- "lev_va"
      }
      message("All data in", value_col_init, "are NA, switching to: ", value_col_per)
    }
    
    if(!all(c(date_col_per, value_col_per, approved_col_per) %in% names(gwl_data))){
      stop("gwl_data data frame doesn't include all specified columns")
    }
    
    gwl_data$year <- as.numeric(format(gwl_data[[date_col_per]], "%Y")) + 
      as.numeric(as.character(gwl_data[[date_col_per]], "%j"))/365
    
  }
  
  if(includes_dv){

    date_col_dv <- ifelse(is.na(date_col[1]), "Date", date_col[1])
    value_col_dv <- get_value_column(parameter_cd, gw_level_dv, value_col[1])
    approved_dv <- ifelse(is.na(approved_col[1]),
                          paste0(value_col_dv, "_cd"),
                          approved_col[1])   
    
    if(!all(c(date_col_dv, value_col_dv, approved_dv) %in% names(gw_level_dv))){
      stop("gw_level_dv data frame doesn't include all specified columns")
    }
    
    #Convert date column to date just in case its a POSIXct:
    gw_level_dv[[date_col_dv]] <- as.Date(gw_level_dv[[date_col_dv]])
    
  }
  
  linetype = c('solid', 'dashed')
  
  if(includes_dv){
    complete_df <- data.frame(Date = seq.Date(from = min(gw_level_dv[[date_col_dv]], na.rm = TRUE),
                                       to = max(gw_level_dv[[date_col_dv]], na.rm = TRUE), 
                                       by = "day"))

    gw_lev <- gw_level_dv[ , c(date_col_dv, value_col_dv, approved_dv)]
    names(gw_lev) <- c("Date", "Value", "Approve")
    gw_complete <- complete_df %>% 
      dplyr::left_join(gw_lev, 
                by = "Date") %>% 
      dplyr::mutate(year = as.numeric(format(Date, "%Y")) + 
                              as.numeric(as.character(Date, "%j"))/365,
                     is_na_before = is.na(dplyr::lag(Value)),
                     is_na_after = is.na(dplyr::lead(Value)),
                     is_point = is_na_after & is_na_before & !is.na(Value),
                     is_complete = !is.na(Value) & !is_na_after & !is_na_before,
                     Approve = ifelse(grepl(pattern = "A",  
                                          x =  Approve), "A", "P"))

    gw_complete[is.na(gw_complete$Value), "Approve"] <- "A"

    plot_out <- ggplot() +
      geom_path(data = gw_complete,
                aes(x = year, color = Approve, y = Value)) 
    
    if(sum(gw_complete$is_point) > 0){
      plot_out <- plot_out +
        geom_point(data = dplyr::filter(gw_complete, is_point),
                 aes(x = year, color = Approve, y = Value), size = 0.2) 
    }
    
    plot_out <- plot_out +
      scale_color_manual(ifelse(includes_both,"Daily Data", "EXPLANATION\nDaily Data"),
                         values = c("A" = "blue", "P" = "red"), 
                         labels = c("A" = "Approved",
                                    "P" = "Provisional"))
  } else {
    plot_out <- ggplot()
    
  }
  
  if(includes_gwl){
    plot_out <- plot_out +
      geom_point(data = gwl_data,
                 aes_string(x = "year", 
                            y = value_col_per, 
                            fill = approved_col_per),
                 size = 1.5, shape = 21, color = "transparent") +
      scale_fill_manual("EXPLANATION\nWater-Level\nMeasurement",
                        values = c("A" = "navy", "P" = "red"), 
                        labels = c("A" = "Approved",
                                   "P" = "Provisional"))
  } 
  
  plot_out <- plot_out +
    hasp_framework("Years", y_label, plot_title = plot_title)  +
    scale_x_continuous(sec.axis = dup_axis(labels =  NULL,
                                           name = NULL)) 
  
  if(add_trend & includes_dv){
    
    gw_monthly <- monthly_mean(gw_level_dv, 
                               date_col = date_col_dv, 
                               value_col = value_col_dv)
    
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
  
  if(flip_y){
    plot_out <- plot_out +
      scale_y_continuous(trans = "reverse")
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

filter_pcode <- function(df, pcode){
  
  if("parameter_cd" %in% names(df)){
    if(!all(is.na(pcode))){
      pcode <- dataRetrieval::zeroPad(pcode, 5)
      df <- df[!is.na(df$parameter_cd) & 
                 df$parameter_cd %in% pcode, ]
    } else if(all(is.na(pcode)) &
              length(unique(df$parameter_cd)) > 1){
      warning("Multiple parameter codes detected in column 'parameter_cd',
            and a parameter code is not specified in 'parameter_cd'")
    }
  } else {
    if(!all(is.na(pcode))){
      message("gwl_data data frame does not contain a 'parameter_cd' column,
            yet 'parameter_cd' is defined. Ignoring 'parameter_cd' argument.")
    }
  }
  return(df)
}

