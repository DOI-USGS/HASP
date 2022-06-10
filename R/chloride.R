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
                       CharacteristicName = c("Chloride"),
                       norm_range = c(225,999),
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
  
  qw_sub$year <- as.numeric(format(as.Date(qw_sub$ActivityStartDate), "%Y")) +
    as.numeric(as.character(as.Date(qw_sub$ActivityStartDate), "%j"))/365

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
    
    qw_sub <- qw_sub[, c("ActivityStartDateTime", "year", "ResultMeasureValue", "condition")]

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
  
  seg_df <- create_segs(qw_sub,
                        value_col = "ResultMeasureValue",
                        date_col = "ActivityStartDateTime")
  
  linetype = c('solid', 'dashed')
  
  plot_out <- ggplot() +
    geom_point(data = qw_sub,
               aes(x = year, y = ResultMeasureValue,
                   shape = condition, 
                   color = condition)) +
    geom_segment(data = seg_df, color = "blue", 
                 aes(x = x1, xend = x2, 
                     y = y1, yend = y2,
                     group = trend, linetype = trend)) +
    hasp_framework("Date", y_label, plot_title, 
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
                          breaks = c("5-year trend", "20-year trend"),
                          labels = c("5 year", "20 year")) +
    guides(shape = guide_legend(order = 1),
           color = guide_legend(order = 1),
           linetype = guide_legend(order = 2)) 
  
  if(include_table){
    
    trend_results <- kendell_test_5_20_years(qw_sub, seasonal = FALSE,
                                             date_col = "ActivityStartDateTime", 
                                             value_col = "ResultMeasureValue",
                                             enough_5 = 1, enough_20 = 1)
    
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

create_segs <- function(x, 
                        date_col = "sample_dt", 
                        value_col = "result_va",
                        enough_5 = 1, enough_20 = 1){
  
  trend_results <- kendell_test_5_20_years(x, seasonal = FALSE,
                                           enough_5 = enough_5, enough_20 = enough_20,
                                           date_col = date_col, 
                                           value_col = value_col)

  df_seg <- data.frame(x1 = as.Date(c(NA, NA)),
                       x2 = rep(max(as.Date(x[[date_col]]), na.rm = FALSE), 2),
                       y1 = c(NA, NA),
                       y2 = c(NA, NA),
                       trend = trend_results$test, 
                       years = as.numeric(gsub("-year trend", "", trend_results$test)),
                       stringsAsFactors = FALSE)

  for(i in seq_len(nrow(trend_results))){
    if(!is.na(trend_results$trend[i]) && trend_results$trend[i] != "Not significant"){
      df_seg$x1[df_seg$trend == trend_results$test[i]] <- as.Date(df_seg$x2[df_seg$trend == trend_results$test[i]] - as.difftime(df_seg$years[i]*365+1, units = "days"), origin = "1970-01-01")
      
      df_seg$y1[df_seg$trend == trend_results$test[i]] <- as.numeric(df_seg$x1[df_seg$trend == trend_results$test[i]])*trend_results$slope[i] + trend_results$intercept[i]
      df_seg$y2[df_seg$trend == trend_results$test[i]] <- as.numeric(df_seg$x2[df_seg$trend == trend_results$test[i]])*trend_results$slope[i] + trend_results$intercept[i]
    } 
  }

  df_seg <- df_seg[!(is.na(df_seg$y1)),]
  
  if(nrow(df_seg) != 0){
    df_seg$x2 <- as.numeric(format(df_seg$x2, "%Y"))
    df_seg$x1 <- as.numeric(format(df_seg$x1, "%Y"))    
  }

  return(df_seg)
  
}

