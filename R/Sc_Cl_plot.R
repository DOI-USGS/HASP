#' Specific conductance and chloride
#'
#' Functions to create the individual chloride, specific conductance, 
#' and combination plots and tables for a single site.
#'
#' @param qw_data data frame returned from dataRetrieval::readNWISqw,
#' must include columns sample_dt, parm_cd, result_va
#' @param plot_title character
#' @rdname sc_cl
#' @export
#' @import ggplot2
#' @importFrom scales comma
#' @importFrom ggpmisc stat_poly_eq
#' @importFrom dataRetrieval readNWISpCode
#' @examples 
#' 
#' # site <- "263819081585801"
#' # parameterCd <- c("00095","90095","00940","99220")
#' # site_data <- dataRetrieval::readNWISqw(site, 
#' #                                        parameterCd)
#' # Using package example data:
#' qw_data <- L2701_example_data$QW
#' title <- paste(attr(qw_data, "siteInfo")[["station_nm"]], ": Specific Conductance vs Chloride")
#' Sc_Cl_plot(qw_data, plot_title = title)
Sc_Cl_plot <- function(qw_data, plot_title){
  
  chloride <- sp <- ..eq.label.. <- ..rr.label.. <- ".dplyr"
  
  # Specify the plot titles using the function getParmCodeDef
  
  Cltitle <- trimmed_name("99220")
  Sctitle <- trimmed_name("90095")
  
  Plotdata <- Sc_Cl_table(qw_data)
  
  if(nrow(Plotdata) == 0){
    stop("No data to make plot")
  }

  plot_out <- ggplot(data = Plotdata,
                     aes(x = sp, y = chloride)) +
    geom_point(color = "blue") +
    stat_smooth(method = "lm", color = "black", 
                formula = y ~ x , se = FALSE) +
    theme_gwl() +
    stat_poly_eq(formula = y ~ x,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) +
    scale_y_continuous(Cltitle, 
                       labels = scales::comma) +
    scale_x_continuous(Sctitle, 
                       labels = scales::comma) +
    labs(caption = paste("Plot created:", Sys.Date())) +
    ggtitle(plot_title, subtitle = "U.S. Geological Survey") 
  
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
  
  if(!all(c("sample_dt", "parm_cd", "result_va") %in% names(qw_data))){
    stop("data frame qw_data doesn't include all mandatory columns")
  }
  
  sample_dt <- startDateTime <- site_no <- parm_cd <- remark_cd <- result_va <- `90095` <- `99220` <- ".dplyr"
  chloride <- sp <- ".dplyr"
  
  Plotdata <- qw_data %>% 
    select(Date = sample_dt, 
           parm_cd, 
           result_va) %>% 
    mutate(parm_cd = ifelse(parm_cd == "00940", "99220", parm_cd),
           parm_cd = ifelse(parm_cd == "00095", "90095", parm_cd)) %>% 
    pivot_wider(names_from = parm_cd, 
                values_from = result_va,
                values_fn = list(result_va = mean_no_na)) %>%  
    rename(chloride = `99220`,
           sp = `90095`) %>% 
    filter(!is.na(sp),
           !is.na(chloride))
  
  return(Plotdata)
  
}

#' @rdname sc_cl
#' @param pcode character pcode to plot
#' @export
#' @examples
#' title <- attr(qw_data, "siteInfo")[["station_nm"]]
#' qw_plot(qw_data, title, pcode = c("00095", "90095"))
#' qw_plot(qw_data, title, pcode = c("00940","99220"))
qw_plot <- function(qw_data, plot_title,
                    pcode = c("00095", "90095")){
  
  if(!all(c("sample_dt", "result_va", "remark_cd", "parm_cd") %in% names(qw_data))){
    stop("data frame qw_data doesn't include all mandatory columns")
  }
  
  sample_dt <- result_va <- remark_cd <- parm_cd <- ".dplyr"
  
  qw_data <- qw_data %>% 
    filter(parm_cd %in% pcode)
  
  y_label <- trimmed_name(pcode[1])

  plot_out <- ggplot() +
    geom_point(data = qw_data,
               aes(x = sample_dt, y = result_va),
               size = 1.5, color = "blue") +
    theme_gwl() +
    labs(caption = paste("Plot created:", Sys.Date()), 
         y = y_label, x = "Date") +
    expand_limits(y = 0) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +

    ggtitle(plot_title, 
            subtitle = "U.S. Geological Survey") +
    theme(legend.position = "bottom",
          legend.direction = "vertical")
  
  return(plot_out)
  
}

#' @rdname sc_cl
#' @param norm_range a numerical range to potentially group the data. If NA, no grouping is shown.
#' @export
#' @examples
#' 
#' qw_summary(qw_data, pcode = c("00940","99220"), norm_range = c(225,999))
#' qw_summary(qw_data, pcode = c("00095","90095"), norm_range = NA)
qw_summary <- function(qw_data, pcode, 
                       norm_range = NA){
  
  if(!all(c("sample_dt", "result_va", "remark_cd", "parm_cd") %in% names(qw_data))){
    stop("data frame qw_data doesn't include all mandatory columns")
  }

  p_code_info <- dataRetrieval::readNWISpCode(pcode)
  unit_meas <- p_code_info$parameter_units[1]
  
  sample_dt <- result_va <- remark_cd <- parm_cd <- ".dplyr"
  
  qw_sub <- qw_data %>% 
    filter(parm_cd %in% pcode) %>% 
    arrange(sample_dt)
  
  qw_info <- data.frame(
        first_sample = min(qw_sub$sample_dt, na.rm = TRUE),
        first_sample_result = qw_sub$result_va[1],
        last_sample = max(qw_sub$sample_dt, na.rm = TRUE),
        last_sample_result = qw_sub$result_va[nrow(qw_sub)]
      ) %>%  
    bind_cols(site_data_summary(qw_sub, "result_va"))
  
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
     first_day_mid <- qw_sub$sample_dt[qw_sub$result_va >= norm_range[1] & 
                                   qw_sub$result_va <= norm_range[2]]
    
     if(length(first_day_mid) != 0){
       Result[5] <- first_day_mid
     }
     
     first_day_max <- qw_sub$sample_dt[qw_sub$result_va >= norm_range[2]]
     
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


trimmed_name <- function(pcode){
  
  lab_trimmed <- gsub(", unfiltered", "",readNWISpCode(pcode[1])[["parameter_nm"]])
  lab_trimmed <- gsub(", filtered", "", lab_trimmed)
  lab_trimmed <- gsub(", water", "", lab_trimmed)
  lab_trimmed <- gsub(", laboratory", "", lab_trimmed)
  
  return(lab_trimmed)
}
