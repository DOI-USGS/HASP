#' Chloride plot with trend
#'
#' Function creates the cloride over time plot with trends.
#'
#' @param qw_data data frame returned from dataRetrieval::readWQPqw,
#' must include columns ActivityStartDateTime, CharacteristicName, result_va
#' @param CharacteristicName character CharacteristicName to plot
#' @param norm_range a numerical range to potentially group the data. If NA, no grouping is shown.
#' @param plot_title character title for plot
#' @param y_label character label for y axis. If left as NA, the function
#' will attempt to use the "variableInfo" attribute of qw_data. This is
#' attached to dataRetrieval output.
#' @param subtitle character. Sub-title for plot, default is "U.S. Geological Survey".
#' @param n_years integer. This is the number of years to calculate the trend on.
#' Default is 10. This can be a vector of years.
#' @param POR_trend a logical indicating whether to include a trend test
#' for the full period of record. Default is \code{TRUE}.
#' @param include_table logical whether or not to include the trend table in the upper left corner.
#' @rdname chloridetrend
#' @export
#' @import ggplot2
#' @examples 
#' 
#' # site <- "263819081585801"
#' # parameterCd <- c("00095","90095","00940","99220")
#' # site_data <- dataRetrieval::readWQPqw(site, 
#' #                                        parameterCd)
#' # Using package example data:
#' qw_data <- L2701_example_data$QW
#' plot_title <- paste(attr(qw_data, "siteInfo")[["station_nm"]], ": Chloride")
#' trend_plot(qw_data, plot_title)
trend_plot <- function(qw_data, plot_title,
                       y_label = NA, 
                       n_years = 10,
                       POR_trend = TRUE,
                       CharacteristicName = c("Chloride"),
                       norm_range = c(230, 860),
                       subtitle = "U.S. Geological Survey",
                       include_table = TRUE){
  
  if(!all(c("ActivityStartDateTime", "ResultMeasureValue", "CharacteristicName") %in% names(qw_data))){
    stop("data frame qw_data doesn't include all mandatory columns")
  }
  
  if(all(!is.na(norm_range)) && length(norm_range) != 2){
    stop("norm_range vector needs to be of length 2")
  }

  qw_sub <- qw_data[qw_data$CharacteristicName %in% CharacteristicName, ]
  qw_sub <- qw_sub[order(qw_sub$ActivityStartDateTime), ]
  
  if(!"ActivityStartDate" %in% names(qw_sub)){
    qw_sub$ActivityStartDate <- as.Date(qw_sub$ActivityStartDateTime)
  }
  
  if(all(is.na(qw_sub$ActivityStartDate))){
    qw_sub$ActivityStartDate <- as.Date(qw_sub$ActivityStartDate)
  }
  
  qw_sub$year <- as.numeric(format(as.Date(qw_sub$ActivityStartDate), "%Y")) +
    as.numeric(format(as.Date(qw_sub$ActivityStartDate), "%j"))/365

  if(all(is.na(norm_range))){
    qw_sub$condition <- "medium"
    col_values <- c("medium")
    col_labels <- c("Medium")
  } else {
    # TODO: think about ways to incorporate NA in the norm_range?
    # for example, c(225, NA) could be only things lower than 225 are
    # coded a different color and there's no upper bound?
    qw_sub$condition <- ifelse(qw_sub$ResultMeasureValue < norm_range[1], "low",
                               ifelse(qw_sub$ResultMeasureValue >= norm_range[1] & 
                                 qw_sub$ResultMeasureValue < norm_range[2], "medium",
                               ifelse(qw_sub$ResultMeasureValue >= norm_range[2] , "high", NA_character_)))
    
    qw_sub <- qw_sub[, c("ActivityStartDate", "year", "ResultMeasureValue", "condition")]

    col_values <- c("low", "medium", "high")
    col_labels <- c(paste0("<", norm_range[1]), 
                    paste0(">=", norm_range[1], " and <", norm_range[2]), 
                    paste0(">=",norm_range[2]))
    
  }
  
  if(is.na(y_label)){
    y_label <- attr(qw_data, "variableInfo")
    if(is.null(y_label)){
      y_label <- ""
    } else {
      y_label <- y_label[y_label$characteristicName %in% CharacteristicName,]
      y_label <- trimmed_name(y_label$parameter_nm)      
    }
  }
  
  on_top <- zero_on_top(qw_sub$ResultMeasureValue)
  
  trend_results <- trend_test(gw_level_dv = NULL,
                              gwl_data = qw_sub,
                              date_col = "ActivityStartDate",
                              value_col = "ResultMeasureValue", 
                              approved_col = "condition",
                              n_years = n_years,
                              POR_trend = POR_trend)
  
  seg_df <- create_segs(trend_results,
                        qw_sub,
                        value_col = "ResultMeasureValue",
                        date_col = "ActivityStartDate")
  
  linetype = c('solid', 'dashed')
  
  plot_out <- ggplot() +
    geom_point(data = qw_sub,
               aes(x = year, y = ResultMeasureValue,
                   shape = condition, 
                   color = condition)) 
  
  if(!is.null(seg_df)){
    plot_out <- plot_out +
      geom_segment(data = seg_df, color = "blue", 
                   aes(x = x1, xend = x2, 
                       y = y1, yend = y2,
                       group = years, linetype = years))
  }
  
  plot_out <- plot_out +
    hasp_framework("Date", y_label, plot_title, 
                   subtitle = subtitle,
                   zero_on_top = on_top, include_y_scale = TRUE) +
    scale_x_continuous(sec.axis = dup_axis(labels =  NULL,
                                           name = NULL)) +
    scale_color_manual(name = "EXPLANATION", 
                       breaks = col_values,
                       labels = col_labels,
                       values = c("blue", "orange", "red")) +
    scale_shape_manual(name = "EXPLANATION", 
                       breaks = col_values,
                       labels = col_labels,
                       values = c(24, 23, 22)) +
    scale_linetype_manual("Trend", 
                          values = linetype,
                          breaks = trend_results$test,
                          labels = trend_results$test) +
    guides(shape = guide_legend(order = 1),
           color = guide_legend(order = 1),
           linetype = guide_legend(order = 2)) 
  
  if(include_table){
    
    trend_results$tau <- signif(trend_results$tau, digits = 4)
    trend_results$pValue <- signif(trend_results$pValue, digits = 4)
    trend_results$slope <- signif(trend_results$slope, digits = 4)
    trend_results$intercept <- signif(trend_results$intercept, digits = 4)
    
    plot_out <- plot_out +
      ggpp::geom_table(aes(x = -Inf, y = Inf,
                           label = list(trend_results)),
                       vjust = 1, hjust = 0)
  }
    
  return(plot_out)
  
}

create_segs <- function(trend_results,
                        x, 
                        date_col = "sample_dt", 
                        value_col = "result_va"){

  if(all(trend_results$trend == "Insufficient data")){
    return(NULL)
  }
  POR <- as.character(diff(range(decimalDate(as.Date(x[[date_col]])))))
              
  df_seg <- trend_results %>%
    dplyr::rename(years = test) %>%
    dplyr::mutate(n_year = as.numeric(dplyr::if_else(grepl("-year trend", years), 
                                          gsub("-year trend", "", years),
                                          POR)),
                  x2 = max(as.Date(x[[date_col]]), na.rm = FALSE),
                  x1 = as.Date(x2 - as.difftime(n_year * 365 + 1,
                                                units = "days"),
                               origin = "1970-01-01"),
                  y1 = decimalDate(x1) * slope + intercept,
                  y2 = decimalDate(x2) * slope + intercept) %>%
    dplyr::filter(!is.na(y2),
                  trend != "Not significant") %>%
    dplyr::mutate(x2 = decimalDate(x2),
                  x1 = decimalDate(x1))

  return(df_seg)
  
}

