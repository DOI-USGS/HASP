#' Chloride plot with trend
#'
#' Function creates the cloride over time plot with trends.
#'
#' @param qw_data data frame returned from dataRetrieval::readNWISqw,
#' must include columns sample_dt, parm_cd, result_va
#' @param pcode character pcode to plot
#' @param norm_range a numerical range to potentially group the data. If NA, no grouping is shown.
#' @param plot_title character
#' @rdname chloridetrend
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
#' title <- paste(attr(qw_data, "siteInfo")[["station_nm"]], ": Chloride")
#' trend_plot(qw_data, plot_title = title)
trend_plot <- function(qw_data, plot_title, 
                          pcode = c("00940","99220"),
                          norm_range = c(225,999)){
  
  if(!all(c("sample_dt", "result_va", "remark_cd", "parm_cd") %in% names(qw_data))){
    stop("data frame qw_data doesn't include all mandatory columns")
  }
  
  if(all(!is.na(norm_range)) && length(norm_range) != 2){
    stop("norm_range vector needs to be of length 2")
  }
  
  sample_dt <- condition <- result_va <- remark_cd <- parm_cd <- ".dplyr"
  x1 <- x2 <- y1 <- y2 <- trend <- ".dplyr"
  
  qw_sub <- qw_data %>% 
    filter(parm_cd %in% pcode) %>% 
    arrange(sample_dt)

  if(all(is.na(norm_range))){
    qw_sub$condition <- "medium"
    col_values <- c("medium")
    col_labels <- c("Medium")
  } else {
    # TODO: think about ways to incorporate NA in the norm_range?
    # for example, c(225, NA) could be only things lower than 225 are
    # coded a different color and there's no upper bound?
    qw_sub <- qw_sub %>% 
      mutate(condition = case_when(
        result_va < norm_range[1] ~ "low",
        result_va >= norm_range[1] & 
          result_va <= norm_range[2] ~ "medium",
        result_va > norm_range[2] ~ "high")) %>% 
      select(sample_dt, result_va, condition)
    
    col_values <- c("low", "medium", "high")
    col_labels <- c(paste0("<", norm_range[1]), 
                    paste0(">=", norm_range[1], " and <", norm_range[2]), 
                    paste0(">=",norm_range[2]))
    
  }
  
  y_label <- trimmed_name(pcode[1])

  seg_df <- create_segs(qw_sub)
  linetype = c('solid', 'dashed')
  
  plot_out <- ggplot() +
    geom_point(data = qw_sub,
               aes(x = sample_dt, y = result_va,
                   shape = condition, 
                   color = condition)) +
    geom_segment(data = seg_df, color = "blue", 
                 aes(x = x1, xend = x2, 
                     y = y1, yend = y2,
                     group = trend, linetype = trend)) +
    theme_gwl() +
    expand_limits(y = 0) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(caption = paste("Plot created:", Sys.Date()), 
         y = y_label, x = "Date") +
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
    ggtitle(plot_title, 
            subtitle = "U.S. Geological Survey") +
    guides(shape = guide_legend(order = 1),
           color = guide_legend(order = 1),
           linetype = guide_legend(order = 2))
    
    return(plot_out)
  
}

create_segs <- function(x, trend_results){
  
  trend_results <- kendell_test_5_20_years(x, seasonal = FALSE,
                                           enough_5 = 1, enough_20 = 1,
                                           date_col = "sample_dt", 
                                           value_col = "result_va")

  df_seg <- data.frame(x1 = as.Date(c(NA, NA)),
                       x2 = as.Date(c(NA, NA)),
                       y1 = c(NA, NA),
                       y2 = c(NA, NA),
                       trend = c("",""), 
                       stringsAsFactors = FALSE)
  
  
  names(df_seg) <- c("x1", "x2", "y1", "y2", "trend")
  
  df_seg$x2 <- x$sample_dt[nrow(x)]
  df_seg$x1[1] <- as.Date(df_seg$x2[1] - as.difftime(5*365+1, units = "days"), origin = "1970-01-01")
  df_seg$x1[2] <- as.Date(df_seg$x2[2] - as.difftime(20*365+5, units = "days"), origin = "1970-01-01")
  
  df_seg$y1[1] <- as.numeric(df_seg$x1[1])*trend_results$slope[trend_results$test == "5-year trend"] + trend_results$intercept[trend_results$test == "5-year trend"]
  df_seg$y2[1] <- as.numeric(df_seg$x2[1])*trend_results$slope[trend_results$test == "5-year trend"] + trend_results$intercept[trend_results$test == "5-year trend"]
  df_seg$trend[1] <- "5-year trend"
  
  df_seg$y1[2] <- as.numeric(df_seg$x1[2])*trend_results$slope[trend_results$test == "20-year trend"] + trend_results$intercept[trend_results$test == "20-year trend"]
  df_seg$y2[2] <- as.numeric(df_seg$x2[2])*trend_results$slope[trend_results$test == "20-year trend"] + trend_results$intercept[trend_results$test == "20-year trend"]
  df_seg$trend[2] <- "20-year trend"
  
  return(df_seg)
  
}

