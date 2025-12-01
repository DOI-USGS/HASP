#' Specific conductance and chloride
#'
#' Functions to create the individual chloride, specific conductance, 
#' and combination plots and tables for a single site.
#'
#' @param qw_data data frame returned from \code{\link[dataRetrieval]{readWQPqw}},
#' must include columns "ActivityStartDateTime", "CharacteristicName", and "ResultMeasureValue"
#' @param plot_title character title for plot
#' @param subtitle character. Sub-title for plot, default is "U.S. Geological Survey".
#' @rdname sc_cl
#' @export
#' @import ggplot2
#' @examples 
#' 
#' # site <- "USGS-263819081585801"
#' # parameterCd <- c("00095","90095","00940","99220")
#' # site_data <- dataRetrieval::readWQPqw(site, 
#' #                                        parameterCd)
#' # Using package example data:
#' qw_data <- L2701_example_data$QW
#' plot_title <- paste("USGS-263819081585801",
#'  ": Specific Conductance vs Chloride")
#' Sc_Cl_plot(qw_data, plot_title)
Sc_Cl_plot <- function(qw_data, 
                       plot_title,
                       subtitle = "U.S. Geological Survey"){
  

  # Specify the plot titles using the function getParmCodeDef
  
  Cltitle <- trimmed_name(dataRetrieval::read_waterdata_parameter_codes(parameter_code = "99220")[["parameter_name"]])
  Sctitle <- trimmed_name(dataRetrieval::read_waterdata_parameter_codes(parameter_code = "90095")[["parameter_name"]])
  
  Plotdata <- Sc_Cl_table(qw_data)
  
  if(nrow(Plotdata) == 0){
    stop("No data to make plot")
  }

  plot_out <- ggplot(data = Plotdata,
                     aes(x = `Specific conductance`, y = Chloride)) +
    geom_point(color = "blue") +
    stat_smooth(method = "lm", color = "black", 
                formula = y ~ x , se = FALSE) +
    hasp_framework(y_label = Cltitle, 
                   x_label = Sctitle, include_y_scale = TRUE,
                   subtitle = subtitle,
                   plot_title = plot_title,
                   zero_on_top = NA) +
    scale_x_continuous(sec.axis = dup_axis(labels =  NULL,
                                           name = NULL)) +
    ggpmisc::stat_poly_eq(formula = y ~ x,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) 
  
  return(plot_out)
  
  
}

#' @rdname sc_cl
#' @export
#' @examples 
#'
#' sc_cl <- Sc_Cl_table(qw_data)
Sc_Cl_table <- function(qw_data){
  
  mean_no_na <- function(x){
    mean(x, na.rm = TRUE)
  }
  
  if(!all(c("ActivityStartDateTime", "CharacteristicName", "ResultMeasureValue") %in% names(qw_data))){
    stop("data frame qw_data doesn't include all mandatory columns")
  }
  
  
  Plotdata <- qw_data |> 
    dplyr::select(Date = ActivityStartDateTime, 
           CharacteristicName, 
           ResultMeasureValue) |> 
    dplyr::filter(!is.na(ResultMeasureValue)) |>
    tidyr::pivot_wider(names_from = CharacteristicName, 
                values_from = ResultMeasureValue,
                values_fn = list(ResultMeasureValue = mean_no_na)) 
  
  return(Plotdata)
  
}

#' @rdname sc_cl
#' @param CharacteristicName character CharacteristicName to filter to.
#' @param y_label character label for y axis. If left as NA, the function
#' will attempt to use the "variableInfo" attribute of qw_data. This is
#' attached to dataRetrieval output.
#' @param start_date Date to start plot. If \code{NA} (which is the default),
#' the plot will start at the earliest measurement.
#' @param end_date Date to end plot. If \code{NA} (which is the default), 
#' the plot will end with the latest measurement. 
#' @param subtitle character. Sub-title for plot, default is "U.S. Geological Survey".
#' @export
#' @examples
#' plot_title <- paste("USGS-263819081585801",
#'  ": Specific Conductance vs Chloride")
#' qw_plot(qw_data, plot_title, CharacteristicName = "Chloride")
#' qw_plot(qw_data, plot_title, CharacteristicName = "Specific conductance")
#' qw_plot(qw_data,
#'         plot_title, 
#'         CharacteristicName = "Specific conductance",
#'         start_date = "1990-01-01")
#'        
#' site <- "USGS-01491000"
#' qw_data_phos <- dataRetrieval::readWQPqw(site, "Orthophosphate")
#' qw_plot(qw_data_phos ,
#'         CharacteristicName = "Orthophosphate",
#'         plot_title = "Choptank: Orthophosphate")
qw_plot <- function(qw_data, plot_title,
                    y_label = NA,
                    CharacteristicName = "Chloride",
                    start_date = NA,
                    end_date = NA, 
                    subtitle = "U.S. Geological Survey"){
  
  if(!all(c("ActivityStartDateTime", "CharacteristicName",
            "ResultMeasureValue", "ResultDetectionConditionText") %in% names(qw_data))){
    stop("data frame qw_data doesn't include all mandatory columns")
  }

  qw_data <- qw_data |> 
    dplyr::filter(CharacteristicName %in% !!CharacteristicName)  |> 
    dplyr::mutate(year = as.numeric(format(as.Date(ActivityStartDate), "%Y")) + 
                    as.numeric(format(as.Date(ActivityStartDate), "%j"))/365)
  
  if(!is.na(start_date)){
    qw_data <- qw_data[qw_data$ActivityStartDate >= as.Date(start_date) ,]
  }
  
  if(!is.na(end_date)){
    qw_data <- qw_data[qw_data$ActivityStartDate <= as.Date(end_date) ,]
  }
  
  qw_data$qualifier <- FALSE
  suppressWarnings(qw_data$qualifier[is.na(as.numeric(qw_data$ResultMeasureValue)) & 
                      !is.na(qw_data$ResultMeasureValue)] <- TRUE)

  qw_data$DetectCondition <- toupper(qw_data$ResultDetectionConditionText)
  qw_data$qualifier[grep("NON-DETECT",qw_data$DetectCondition)] <- TRUE
  qw_data$qualifier[grep("NON DETECT",qw_data$DetectCondition)] <- TRUE
  qw_data$qualifier[grep("NOT DETECTED",qw_data$DetectCondition)] <- TRUE
  qw_data$qualifier[grep("DETECTED NOT QUANTIFIED",qw_data$DetectCondition)] <- TRUE
  qw_data$qualifier[grep("BELOW QUANTIFICATION LIMIT",qw_data$DetectCondition)] <- TRUE
  
  any_censored <- any(qw_data$qualifier)
  qw_data$qualifier <- ifelse(qw_data$qualifier, "Not Detected", "Detected")
  
  if(is.na(y_label)){
    y_label <- attr(qw_data, "variableInfo")
    if(is.null(y_label)){
      y_label <- ""
    } else {
      y_label <- y_label[y_label$characteristicName %in% CharacteristicName,]
      y_label <- trimmed_name(y_label$parameter_nm)      
    }
  }
  
  plot_out <- ggplot() +
    hasp_framework(x_label = "Date", y_label = y_label, 
                   include_y_scale = TRUE,
                   plot_title = plot_title, 
                   zero_on_top = FALSE, 
                   subtitle = subtitle) +
    scale_x_continuous(sec.axis = dup_axis(labels =  NULL,
                                           name = NULL))

  if(any_censored){
    plot_out <- plot_out +
      geom_point(data = qw_data ,
                 aes(x = year, y = ResultMeasureValue, shape = qualifier),
                 size = 1.5, color = "blue") +
      scale_shape_manual(name = NULL,
                         values = c("Not Detected" = 1, "Detected" = 16),
                         drop = FALSE) 
  } else {
    plot_out <- plot_out +
      geom_point(data = qw_data ,
                 aes(x = year, y = ResultMeasureValue),
                 size = 1.5, color = "blue")
  }
 

    
  
  return(plot_out)
  
}

#' @rdname sc_cl
#' @param norm_range a numerical range to potentially group the data. If NA, no grouping is shown.
#' @export
#' @examples
#' 
#' qw_summary(qw_data, CharacteristicName = "Chloride",
#'  norm_range = c(230, 860))
#' qw_summary(qw_data, CharacteristicName = "Specific conductance",
#'  norm_range = NA)
qw_summary <- function(qw_data, CharacteristicName, 
                       norm_range = NA){

  if(!all(c("ActivityStartDateTime", "ResultMeasureValue", 
            "CharacteristicName", "ResultMeasure.MeasureUnitCode") %in% names(qw_data))){
    stop("data frame qw_data doesn't include all mandatory columns")
  }

  qw_sub <- qw_data[qw_data$CharacteristicName %in% CharacteristicName, ]
  qw_sub <- qw_sub[order(qw_sub$ActivityStartDateTime) , ]
  
  unit_meas <- unique(qw_sub$ResultMeasure.MeasureUnitCode)[1]

  qw_info <- data.frame(
        first_sample = min(as.Date(qw_sub$ActivityStartDateTime), na.rm = TRUE),
        first_sample_result = qw_sub$ResultMeasureValue[1],
        last_sample = max(as.Date(qw_sub$ActivityStartDateTime), na.rm = TRUE),
        last_sample_result = qw_sub$ResultMeasureValue[nrow(qw_sub)]
      ) |>  
    dplyr::bind_cols(site_data_summary(qw_sub, 
                                       site_col = "MonitoringLocationIdentifier",
                                       value_col = "ResultMeasureValue"))
                
  Analysis <- c("Date of first sample",
               paste0("First sample result (",unit_meas,")"),
               "Date of last sample",
               paste0("Last sample result (",unit_meas,")"),
               ifelse(all(is.na(norm_range)), "", paste("Date of first sample within ", 
                     norm_range[1],"to", norm_range[2], unit_meas)),
               ifelse(all(is.na(norm_range)), "", paste("Date of first sample with ", 
                     norm_range[2] + 1, unit_meas, "or greater")),
               paste0("Minimum (", unit_meas, ")"),
               paste0("Maximum (", unit_meas, ")"),
               paste0("Mean (", unit_meas, ")"),
               paste0("First quartile (", unit_meas, ")"),
               paste0("Median (", unit_meas, ")"),
               paste0("Third quartile (", unit_meas, ")"),
               "Number of samples")
  
  Result <- c(as.character(qw_info$first_sample),
             signif(qw_info$first_sample_result, 3),
             as.character(qw_info$last_sample),
             signif(qw_info$last_sample_result, 3),
             "",
             "",
             signif(qw_info$min_site, 3),
             signif(qw_info$max_site, 3),
             signif(qw_info$mean_site, 3),
             signif(qw_info$p25, 3),
             signif(qw_info$p50, 3),
             signif(qw_info$p75, 3),
             as.integer(qw_info$count)
             )
  
  if(!all(is.na(norm_range))){
     first_day_mid <- qw_sub$sample_dt[qw_sub$ResultMeasureValue >= norm_range[1] & 
                                   qw_sub$ResultMeasureValue <= norm_range[2]]
    
     if(length(first_day_mid) != 0){
       Result[5] <- first_day_mid
     }
     
     first_day_max <- qw_sub$ResultMeasureValue[qw_sub$ResultMeasureValue >= norm_range[2]]
     
     if(length(first_day_max) != 0){
       Result[6] <- first_day_max
     }
    
  } else {
    Analysis <- Analysis[-5:-6]
    Result <- Result[-5:-6]
  }
  
  df_return <- data.frame(Analysis, 
                          Result, 
                          stringsAsFactors = FALSE)
  
  return(df_return)
  
}


trimmed_name <- function(parameter_nm){
  
  lab_trimmed <- gsub(", unfiltered", "",parameter_nm)
  lab_trimmed <- gsub(", filtered", "", lab_trimmed)
  lab_trimmed <- gsub(", water", "", lab_trimmed)
  lab_trimmed <- gsub(", laboratory", "", lab_trimmed)
  lab_trimmed <- unique(lab_trimmed)
  
  return(lab_trimmed)
}
