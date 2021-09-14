#' Specific conductance and chloride
#'
#' Functions to create the individual chloride, specific conductance, 
#' and combination plots and tables for a single site.
#'
#' @param qw_data data frame returned from dataRetrieval::readWQPqw,
#' must include columns sample_dt, parm_cd, result_va
#' @param plot_title character title for plot
#' @rdname sc_cl
#' @export
#' @import ggplot2
#' @importFrom ggpmisc stat_poly_eq
#' @examples 
#' 
#' # site <- "263819081585801"
#' # parameterCd <- c("00095","90095","00940","99220")
#' # site_data <- dataRetrieval::readWQPqw(site, 
#' #                                        parameterCd)
#' # Using package example data:
#' qw_data <- L2701_example_data$QW
#' plot_title <- paste(attr(qw_data, "siteInfo")[["station_nm"]], ": Specific Conductance vs Chloride")
#' Sc_Cl_plot(qw_data, plot_title)
Sc_Cl_plot <- function(qw_data, plot_title){
  
  Chloride <- `Specific conductance` <- ..eq.label.. <- ..rr.label.. <- ".dplyr"
  
  # Specify the plot titles using the function getParmCodeDef
  
  Cltitle <- trimmed_name(dataRetrieval::readNWISpCode("99220")[["parameter_nm"]])
  Sctitle <- trimmed_name(dataRetrieval::readNWISpCode("90095")[["parameter_nm"]])
  
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
                   plot_title = plot_title,
                   zero_on_top = NA) +
    scale_x_continuous(sec.axis = dup_axis(labels =  NULL,
                                           name = NULL)) +
    stat_poly_eq(formula = y ~ x,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) 
  
  return(plot_out)
  
  
}

#' @rdname sc_cl
#' @export
#' @import dplyr
#' @importFrom tidyr pivot_wider
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
  
  ActivityStartDateTime <- startDateTime <- site_no <- CharacteristicName <- ResultMeasureValue <- ".dplyr"

  Plotdata <- qw_data %>% 
    select(Date = ActivityStartDateTime, 
           CharacteristicName, 
           ResultMeasureValue) %>% 
    filter(!is.na(ResultMeasureValue)) %>%
    pivot_wider(names_from = CharacteristicName, 
                values_from = ResultMeasureValue,
                values_fn = list(ResultMeasureValue = mean_no_na)) 
  
  return(Plotdata)
  
}

#' @rdname sc_cl
#' @param CharacteristicName character CharacteristicName to filter to.
#' @param y_label character label for y axis. If left as NA, the function
#' will attempt to use the "variableInfo" attribute of qw_data. This is
#' attached to dataRetrieval output.
#' @export
#' @examples
#' plot_title <- attr(qw_data, "siteInfo")[["station_nm"]]
#' qw_plot(qw_data, plot_title, CharacteristicName = "Chloride")
#' qw_plot(qw_data, plot_title, CharacteristicName = "Specific conductance")
qw_plot <- function(qw_data, plot_title,
                    y_label = NA,
                    CharacteristicName = "Chloride"){
  
  if(!all(c("ActivityStartDateTime", "CharacteristicName", "ResultMeasureValue") %in% names(qw_data))){
    stop("data frame qw_data doesn't include all mandatory columns")
  }
  
  ActivityStartDateTime <- ResultMeasureValue <- year <- ".dplyr"
  
  qw_data <- qw_data %>% 
    filter(CharacteristicName %in% !!CharacteristicName)  %>% 
    mutate(year = as.numeric(format(ActivityStartDateTime, "%Y")) + as.numeric(as.character(ActivityStartDateTime, "%j"))/365)
  
  if(is.na(y_label)){
    y_label <- attr(qw_data, "variableInfo")
    if(is.null(y_label)){
      y_label <- ""
    } else {
      y_label <- y_label[y_label$characteristicName %in% CharacteristicName,]
      y_label <- trimmed_name(y_label$parameter_nm)      
    }
  }
  
  on_top <- zero_on_top(qw_data$ResultMeasureValue)
  
  plot_out <- ggplot() +
    geom_point(data = qw_data ,
               aes(x = year, y = ResultMeasureValue),
               size = 1.5, color = "blue") +
    hasp_framework(x_label = "Date", y_label = y_label, include_y_scale = TRUE,
                   plot_title = plot_title, zero_on_top = on_top) +
    scale_x_continuous(sec.axis = dup_axis(labels =  NULL,
                                           name = NULL))
  
  return(plot_out)
  
}

#' @rdname sc_cl
#' @param norm_range a numerical range to potentially group the data. If NA, no grouping is shown.
#' @export
#' @examples
#' 
#' qw_summary(qw_data, CharacteristicName = "Chloride",
#'  norm_range = c(225,999))
#' qw_summary(qw_data, CharacteristicName = "Specific conductance",
#'  norm_range = NA)
qw_summary <- function(qw_data, CharacteristicName, 
                       norm_range = NA){
  
  MonitoringLocationIdentifier <- ".dplyr"
  
  if(!all(c("ActivityStartDateTime", "ResultMeasureValue", "CharacteristicName") %in% names(qw_data))){
    stop("data frame qw_data doesn't include all mandatory columns")
  }

  p_code_info <- attr(qw_data, "variableInfo")
  
  if(is.null(p_code_info)){
    p_code_info <- ""
  } else {
    p_code_info <- p_code_info[p_code_info$characteristicName %in% CharacteristicName,]
  }

  unit_meas <- p_code_info$parameter_units[1]
  
  ActivityStartDateTime <- ResultMeasureValue <- ".dplyr"
  
  qw_sub <- qw_data %>% 
    filter(CharacteristicName %in% !!CharacteristicName) %>% 
    arrange(ActivityStartDateTime)
  
  qw_info <- data.frame(
        first_sample = min(as.Date(qw_sub$ActivityStartDateTime), na.rm = TRUE),
        first_sample_result = qw_sub$ResultMeasureValue[1],
        last_sample = max(as.Date(qw_sub$ActivityStartDateTime), na.rm = TRUE),
        last_sample_result = qw_sub$ResultMeasureValue[nrow(qw_sub)]
      ) %>%  
    bind_cols(site_data_summary(rename(qw_sub,
                                       site_no = MonitoringLocationIdentifier,
                                       value = ResultMeasureValue)))
  
  Analysis = c("Date of first sample",
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
  
  Result = c(as.character(qw_info$first_sample),
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
