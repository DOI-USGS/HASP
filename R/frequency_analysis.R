#' Create a table of monthly frequency analysis
#' 
#' @param gw_level_dv daily groundwater level data frame. Often obtained from from \code{readNWISdv}
#' @param gwl_data data frame returned from dataRetrieval::readNWISgwl, or 
#' data frame with mandatory columns lev_dt (representing date), lev_age_cd (representing
#' approval code), and a column representing the measured value (either lev_va,
#' sl_lev_va, or value).
#' @param parameter_cd If data in gw_level_dv comes from NWIS, the parameter_cd 
#' can be used to define the value_col.
#'  If the data doesn't come directly from NWIS services, this 
#' can be set to \code{NA},and this argument will be ignored.
#' @param date_col the heading of the date column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param value_col name of value column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param approved_col name of column to get provisional/approved status.
#' @param stat_cd If data in gw_level_dv comes from NWIS, the stat_cd 
#' can be used to help define the value_col.
#' @param flip logical. If \code{TRUE}, flips labels so that the lower numbers 
#' are in the higher percentages. Default is \code{TRUE}. 
#' 
#' @return a data frame of monthly groundwater level statistics including the
#' 5th, 10th, 25th, 75th, 90th, and 95th percentiles; the number of
#' years of data; and the lowest monthly median and the highest monthly
#' median.
#' 
#' @export
#' 
#' @examples 
#' 
#' # site <- "263819081585801"
#' p_code_dv <- "62610"
#' statCd <- "00001"
#' # gw_level_dv <- dataRetrieval::readNWISdv(site, p_code_dv, statCd = statCd)
#' gw_level_dv <- L2701_example_data$Daily
#' monthly_frequency <- monthly_frequency_table(gw_level_dv,
#'                                              NULL,
#'                                              parameter_cd = "62610")
#' head(monthly_frequency)
#' gwl_data <- L2701_example_data$Discrete
#' 
#' monthly_frequency_combo <- monthly_frequency_table(gw_level_dv,
#'                                              gwl_data,
#'                                              parameter_cd = "62610")
#' head(monthly_frequency_combo) 
#' monthly_flip <- monthly_frequency_table(gw_level_dv,
#'                                         gwl_data,
#'                                         parameter_cd = "62610",
#'                                         flip = TRUE)
#' head(monthly_flip) 
monthly_frequency_table <- function(gw_level_dv, 
                                    gwl_data, 
                                    parameter_cd = NA,
                                    date_col = NA,
                                    value_col = NA, 
                                    approved_col = NA,
                                    stat_cd = NA,
                                    flip = FALSE) {

  monthly_stats <- stats_by_interval(interval = "month",
                                     gw_level_dv = gw_level_dv, 
                                     gwl_data = gwl_data, 
                                     parameter_cd = parameter_cd,
                                     date_col = date_col,
                                     value_col = value_col, 
                                     approved_col = approved_col,
                                     stat_cd = stat_cd,
                                     flip = flip)

  return(monthly_stats)
  
}


stats_by_interval <- function(interval,
                              gw_level_dv, 
                              gwl_data, 
                              parameter_cd = NA,
                              date_col = NA,
                              value_col = NA, 
                              approved_col = NA,
                              stat_cd = NA,
                              flip = FALSE){
  
  interval <- match.arg(interval, 
                        choices = c("week", "month", "year"), 
                        several.ok = FALSE)
  
  data_list <- set_up_data(gw_level_dv = gw_level_dv, 
                           gwl_data = gwl_data, 
                           parameter_cd = parameter_cd,
                           date_col = date_col,
                           value_col = value_col, 
                           approved_col = approved_col,
                           stat_cd = stat_cd)
  
  gw_level_dv <- data_list$gw_level_dv
  gwl_data <- data_list$gwl_data
  
  gw_level_dv <- dplyr::bind_rows(gw_level_dv,
                                  gwl_data)
  
  gw_level_dv <- gw_level_dv[grepl("A", gw_level_dv$Approve), ]
  
  gw_level_dv$year = as.POSIXlt(gw_level_dv$Date)$year + 1900
  gw_level_dv$month = as.POSIXlt(gw_level_dv$Date)$mon + 1
  gw_level_dv$week = as.POSIXlt(gw_level_dv$Date)$yday%/%7 + 1
  
  gw_level_dv$interval <- gw_level_dv[[interval]]
  
  annual_stats <- gw_level_dv %>%
    dplyr::group_by(year, interval) %>%
    dplyr::summarize(median = stats::median(Value, na.rm = TRUE)) %>%
    dplyr::group_by(interval) %>%
    dplyr::summarize(minMed = min(median, na.rm = TRUE),
                     maxMed = max(median, na.rm = TRUE))
  
  stats <- gw_level_dv %>%    
    dplyr::group_by(year, interval) %>%
    dplyr::summarize(median = stats::median(Value, na.rm = TRUE)) %>%
    dplyr::group_by(interval) %>%
    dplyr::summarize(p5_1 = quantile(median, probs=0.05, type = 6, na.rm=TRUE),
                     p10_1 = quantile(median, probs=0.1, type = 6, na.rm=TRUE),
                     p25_1 = quantile(median, probs=0.25, type = 6, na.rm=TRUE),
                     p50 = quantile(median, probs=0.5, type = 6, na.rm=TRUE),
                     p75_1 = quantile(median, probs=0.75, type = 6, na.rm=TRUE),
                     p90_1 = quantile(median, probs=0.9, type = 6, na.rm=TRUE),
                     p95_1 = quantile(median, probs=0.95, type = 6, na.rm=TRUE),
                     nYears = length(unique(year))) %>%
    dplyr::left_join(annual_stats, by = "interval")
  
  if(flip){
    names(stats)[names(stats) == "p5_1"] <- "p95"
    names(stats)[names(stats) == "p10_1"] <- "p90"
    names(stats)[names(stats) == "p25_1"] <- "p75"
    names(stats)[names(stats) == "p75_1"] <- "p25"
    names(stats)[names(stats) == "p90_1"] <- "p10"
    names(stats)[names(stats) == "p95_1"] <- "p05"
    names(stats)[names(stats) == "minMed"] <- "maxMedium"
    names(stats)[names(stats) == "maxMed"] <- "minMedium"
    names(stats)[names(stats) == "maxMedium"] <- "maxMed"
    names(stats)[names(stats) == "minMedium"] <- "minMed"
  } else {
    names(stats)[names(stats) == "p5_1"] <- "p05"
    names(stats)[names(stats) == "p10_1"] <- "p10"
    names(stats)[names(stats) == "p25_1"] <- "p25"
    names(stats)[names(stats) == "p75_1"] <- "p75"
    names(stats)[names(stats) == "p90_1"] <- "p90"
    names(stats)[names(stats) == "p95_1"] <- "p95"
  }
  
  stats <- stats[, c("interval",
                     "p05", "p10", "p25", "p50",
                     "p75", "p90", "p95",
                     "nYears", "minMed", "maxMed" )]
  
  names(stats)[names(stats) == "interval"] <- interval
  
  return(stats)
}

#' Plot monthly frequency analysis
#' 
#' @param gw_level_dv data frame, daily groundwater level data. Often obtained 
#' from \code{readNWISdv}. Use \code{NULL} for no daily data.
#' @param gwl_data data frame returned from dataRetrieval::readNWISgwl, or 
#' data frame with mandatory columns lev_dt (representing date), lev_age_cd (representing
#' approval code), and a column representing the measured value (either lev_va,
#' sl_lev_va, or value). Use \code{NULL} for no discrete data.
#' @param parameter_cd If data in gw_level_dv comes from NWIS, the parameter_cd 
#' can be used to define the value_col.
#'  If the data doesn't come directly from NWIS services, this 
#' can be set to \code{NA},and this argument will be ignored.
#' @param date_col the heading of the date column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param value_col name of value column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param approved_col name of column to get provisional/approved status.
#' @param plot_range the time frame to use for the plot. Either "Past year" to use the
#' last year of data, or "Calendar year" to use the current calendar year, beginning
#' in January.
#' @param stat_cd If data in gw_level_dv comes from NWIS, the stat_cd 
#' can be used to help define the value_col.
#' @param plot_title the title to use on the plot.
#' @param subtitle character. Sub-title for plot, default is "U.S. Geological Survey".
#' @param y_axis_label the label used for the y-axis of the plot.
#' @param flip logical. If \code{TRUE}, flips the y axis so that the smallest number is on top.
#' Default is \code{TRUE}. 
#' @return a ggplot with rectangles representing the historical monthly percentile,
#' black triangles representing the historical monthly median, and red diamonds
#' showing the last year of groundwater level measurements.
#' 
#' @import ggplot2
#' 
#' @export
#'
#' @examples
#' 
#' # site <- "263819081585801"
#' p_code_dv <- "62610"
#' statCd <- "00001"
#' # gw_level_dv <- dataRetrieval::readNWISdv(site, p_code_dv, statCd = statCd)
#' gw_level_dv <- L2701_example_data$Daily
#' label <- dataRetrieval::readNWISpCode(p_code_dv)[["parameter_nm"]]
#' monthly_frequency <- monthly_frequency_plot(gw_level_dv,
#'                                             gwl_data = NULL,
#'                                             parameter_cd = "62610",
#'                                             plot_title = "L2701 Groundwater Level",
#'                                             y_axis_label = label,
#'                                             flip = FALSE)
#' monthly_frequency
#' 
#' gwl_data <- L2701_example_data$Discrete
#' 
#' monthly_frequency_plot(gw_level_dv,
#'                        gwl_data = gwl_data,
#'                        parameter_cd = "62610",
#'                        plot_title = "L2701 Groundwater Level",
#'                        y_axis_label = label,
#'                        flip = FALSE)
#' 
#' monthly_frequency_flip <- monthly_frequency_plot(gw_level_dv,
#'                                                  gwl_data,
#'                                                  parameter_cd = "62610",
#'                                                  y_axis_label = label,
#'                                                  plot_title = "L2701 Groundwater Level", 
#'                                                  flip = TRUE)
#' monthly_frequency_flip 
#' 
monthly_frequency_plot <- function(gw_level_dv, 
                                   gwl_data, 
                                   parameter_cd = NA,
                                   date_col = NA,
                                   value_col = NA, 
                                   approved_col = NA,
                                   stat_cd = NA,
                                   plot_title = "",
                                   subtitle = "U.S. Geological Survey",
                                   plot_range = c("Past year"),
                                   y_axis_label = "",
                                   flip = FALSE) {
  
  
  data_list <- set_up_data(gw_level_dv = gw_level_dv, 
                           gwl_data = gwl_data, 
                           parameter_cd = parameter_cd,
                           date_col = date_col,
                           value_col = value_col, 
                           approved_col = approved_col,
                           stat_cd = stat_cd)
  
  gw_level_dv <- data_list$gw_level_dv
  gwl_data <- data_list$gwl_data
  
  plot_range <- match.arg(plot_range)
  
  date <- max(c(gw_level_dv$Date,
                gwl_data$Date),
              na.rm = TRUE)
  

  # Calculate the percentiles
  site_statistics <- monthly_frequency_table(gw_level_dv, 
                                             gwl_data,
                                             parameter_cd = NA,
                                             date_col = c("Date", "Date"),
                                             value_col = c("Value", "Value"),
                                             approved_col = c("Approve", "Approve"),
                                             flip = flip)
  
  # Find the bounds of the plot.
  if(plot_range == "Past year") {
    plot_end <- last_day(date) + 1
    plot_start <- first_day(plot_end - 363)
  } else if(plot_range == "Calendar year") {
    calendar_year <- as.POSIXlt(date)$year + 1900
    plot_end <- as.Date(paste0(calendar_year, "-12-31"))
    plot_start <- as.Date(paste0(calendar_year, "-01-01"))
  }
  
  # The last year of groundwater level measurements will plot
  gw_level_dv <- gw_level_dv[gw_level_dv$Date >= plot_start &
                               gw_level_dv$Date <= plot_end, ]

  # Add the first day of the month to the site_statistics table for plotting
  plot_month <- seq(as.Date(plot_start), length = 12, by = "1 month")
  plot_month_lookup <- data.frame(plot_month = plot_month, 
                                  month = as.POSIXlt(plot_month)$mon + 1)
  site_statistics <- dplyr::left_join(site_statistics,
                                      plot_month_lookup, by = "month")
  
  # Set up the plot data for the percentile ranges (rectangle geometry)
  site_statistics_pivot <- site_statistics %>%
    dplyr::select(-month, -nYears, -minMed, -maxMed) %>%
    tidyr::pivot_longer(cols = -plot_month, 
                        names_to = "name", 
                        values_to = "value")
  
  cols <- list(c("p05", "p10"), c("p10", "p25"), c("p25", "p75"),
               c("p75", "p90"), c("p90", "p95"))
  groups <- c("5 - 10", "10 - 25", "25 - 75", "75 - 90", "90 - 95")
  
  plot_list <- data.frame()
  
  for(i in seq_along(cols)) {
    plot_data <- site_statistics_pivot %>%
      dplyr::filter(name %in% cols[[i]]) %>%
      tidyr::pivot_wider(id_cols = plot_month, 
                         names_from = name, 
                         values_from = value) %>%
      dplyr::rename(ymin = cols[[i]][1], ymax = cols[[i]][2]) %>%
      dplyr::mutate(group = groups[i])
    
    plot_list <- dplyr::bind_rows(plot_list, plot_data)
  }
  
  # Make the group an ordered factor so the legend has the correct order
  # and add the last day of the month to draw the rectangles
  site_statistics_plot <- plot_list 
  site_statistics_plot$group <- factor(site_statistics_plot$group,
                                       levels = groups,
                                       ordered = TRUE)
  
  site_statistics_plot$plot_month_last <- last_day(site_statistics_plot$plot_month) + 1

  
  # The median value will plot in the middle of the month
  site_statistics_med <- site_statistics 
  site_statistics_med$group <- "Monthly median"
  site_statistics_med$plot_month_med <- mid_month(site_statistics_med$plot_month)
  
  site_statistics_med <- site_statistics_med[, c("plot_month_med", "p50", "group")]
  names(site_statistics_med) <- c("month", "value", "group")
  points_plot <- gw_level_dv
  
  if(nrow(gw_level_dv) > 0){
    points_plot$group <- "Data point"
    points_plot <- points_plot[, c("Date", "Value", "group")]
    names(points_plot) <- c("month", "value", "group")
    points_plot <- dplyr::bind_rows(site_statistics_med,
                                    points_plot)
  }
  
  # Assign colors and shapes
  rectangle_colors <- c("5 - 10" = "firebrick4",
                        "10 - 25" = "orange2",
                        "25 - 75" = "green2",
                        "75 - 90" = "steelblue1",
                        "90 - 95" = "blue")
  point_shapes <- c("Monthly median" = 17,
                    "Data point" = 18)
  point_colors <- c("Monthly median" = "black",
                    "Data point" = "red")
  
  # Create the plot labels
  start_year <- as.POSIXlt(plot_start)$year + 1900
  end_year <- as.POSIXlt(plot_end)$year + 1900
  if(start_year == end_year) {
    x_label <- as.character(start_year)
  } else {
    x_label <- paste(start_year, end_year, sep = " - ")
  }
  y_label <- y_axis_label
    
  # Plot
  plot_out <- ggplot() +
    geom_rect(data = site_statistics_plot,
              aes(xmin = plot_month,
                  xmax = plot_month_last,
                  ymin = ymin, 
                  ymax = ymax, 
                  fill = group),
              color = "black") +
    geom_vline(xintercept = plot_month) 
  
  if(nrow(points_plot) > 0){
    plot_out <- plot_out +
      geom_point(data = points_plot,
                 aes(x = month,
                     y = value,
                     shape = group,
                     color = group),
                 size = 2.5)
  }
  
  plot_out <- plot_out +
    scale_fill_manual(values = rectangle_colors,
                      name = "Percentile",
                      breaks = c("90 - 95",
                                 "75 - 90",
                                 "25 - 75",
                                 "10 - 25",
                                 "5 - 10")) +
    scale_x_date(limits = c(plot_start, plot_end + 1), expand = c(0,0),
                 breaks = mid_month(plot_month),
                 labels = month.abb[as.POSIXlt(plot_month)$mon + 1]) +
    hasp_framework(x_label = x_label,
                   y_label = y_label,
                   plot_title = plot_title,
                   subtitle = subtitle) +
    theme(axis.ticks.x = element_blank()) +
    guides(color = guide_legend(order = 1, 
                                override.aes = list(shape = point_shapes)),
           shape = "none",
           fill = guide_legend(order = 2))  
  if(flip){
    plot_out <- plot_out +
      scale_y_continuous(trans = "reverse")
  }
  
  plot_out <- plot_out +
    scale_color_manual(values = point_colors, name = "EXPLANATION") +
    scale_shape_manual(values = point_shapes, name = "EXPLANATION") 
  
  return(plot_out)
}

#' Create a table of weekly frequency analysis
#' 
#' The weekly frequency analysis is based on daily values
#' 
#' @param gw_level_dv data frame, daily groundwater level data. Often obtained
#' from \code{readNWISdv}.
#' @param gwl_data data frame returned from dataRetrieval::readNWISgwl, or 
#' data frame with mandatory columns lev_dt (representing date), lev_age_cd (representing
#' approval code), and a column representing the measured value (either lev_va,
#' sl_lev_va, or value).
#' @param flip logical. If \code{TRUE}, flips labels so that the lower numbers 
#' are in the higher percentages. Default is \code{TRUE}. 
#' @param parameter_cd If data in gw_level_dv comes from NWIS, the parameter_cd 
#' can be used to define the value_col.
#'  If the data doesn't come directly from NWIS services, this 
#' can be set to \code{NA},and this argument will be ignored.
#' @param date_col the heading of the date column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param value_col name of value column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param stat_cd If data in gw_level_dv comes from NWIS, the stat_cd 
#' can be used to help define the value_col.
#' @param approved_col name of column to get provisional/approved status.
#' 
#' @return a data frame of weekly frequency analysis
#' 
#' @export
#' 
#' @examples 
#' 
#' # site <- "263819081585801"
#' p_code_dv <- "62610"
#' statCd <- "00001"
#' # gw_level_dv <- dataRetrieval::readNWISdv(site, p_code_dv, statCd = statCd)
#' gw_level_dv <- L2701_example_data$Daily
#' weekly_frequency <- weekly_frequency_table(gw_level_dv,
#'                                            NULL,
#'                                            parameter_cd = "62610")
#' head(weekly_frequency)
#' 
#' gwl_data <- L2701_example_data$Discrete
#' 
#' weekly_frequency <- weekly_frequency_table(gw_level_dv,
#'                                            gwl_data,
#'                                            parameter_cd = "62610") 
#' weekly_frequency
#' weekly_flip <- weekly_frequency_table(gw_level_dv,
#'                                       gwl_data,
#'                                       parameter_cd = "62610",
#'                                       flip = TRUE) 
#' weekly_flip
weekly_frequency_table <- function(gw_level_dv, 
                                   gwl_data,
                                   parameter_cd = NA,
                                   date_col = NA, 
                                   value_col = NA, 
                                   approved_col = NA,
                                   stat_cd = NA,
                                   flip = FALSE) {

  weekly_stats <- stats_by_interval(interval = "week",
                                     gw_level_dv = gw_level_dv, 
                                     gwl_data = gwl_data, 
                                     parameter_cd = parameter_cd,
                                     date_col = date_col,
                                     value_col = value_col, 
                                     approved_col = approved_col,
                                     stat_cd = stat_cd,
                                     flip = flip)
  
  weekly_stats$week_start <- format(as.Date(paste(weekly_stats$week-1, 1, sep="-"), 
                                    "%U-%u"), "%m-%d")
  
  return(weekly_stats)
  
}

#' Plot weekly frequency analysis
#' 
#' The weekly frequency analysis is based on daily data
#' 
#' @param gw_level_dv data frame, daily groundwater level data. Often obtained
#' from \code{readNWISdv}.
#' @param gwl_data data frame returned from dataRetrieval::readNWISgwl, or 
#' data frame with mandatory columns lev_dt (representing date), lev_age_cd (representing
#' approval code), and a column representing the measured value (either lev_va,
#' sl_lev_va, or value).
#' @param parameter_cd If data in gw_level_dv comes from NWIS, the parameter_cd 
#' can be used to define the value_col.
#'  If the data doesn't come directly from NWIS services, this 
#' can be set to \code{NA},and this argument will be ignored.
#' @param date_col the heading of the date column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param value_col name of value column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param approved_col name of column to get provisional/approved status.
#' @param plot_range the time frame to use for the plot. Either "Past year" to use the
#' last year of data, or "Calendar year" to use the current calendar year, beginning
#' in January.
#' @param stat_cd If data in gw_level_dv comes from NWIS, the stat_cd 
#' can be used to help define the value_col.
#' @param plot_title the title to use on the plot
#' @param subtitle character. Sub-title for plot, default is "U.S. Geological Survey".
#' @param y_axis_label the label used for the y-axis of the plot.
#' @param flip logical. If \code{TRUE}, flips the y axis so that the smallest number is on top.
#' Default is \code{FALSE}. 
#' @return a ggplot object with rectangles representing the historical weekly percentiles,
#' and points representing the historical median and daily values
#' 
#' @import ggplot2
#' 
#' @export
#'
#' @examples
#' 
#' # site <- "263819081585801"
#' p_code_dv <- "62610"
#' statCd <- "00001"
#' # gw_level_dv <- dataRetrieval::readNWISdv(site, p_code_dv, statCd = statCd)
#' gw_level_dv <- L2701_example_data$Daily
#' weekly_frequency_plot(gw_level_dv, 
#'                       gwl_data = NULL,
#'                       date_col = "Date",
#'                       value_col = "X_62610_00001",
#'                       approved_col = "X_62610_00001_cd")
#'                       
#' gwl_data <- L2701_example_data$Discrete
#' 
#' weekly_frequency_plot(gw_level_dv, 
#'                       gwl_data = gwl_data,
#'                       parameter_cd = "62610")
#'                       
#' weekly_frequency_plot(gw_level_dv, 
#'                       gwl_data = gwl_data,
#'                       parameter_cd = "62610", 
#'                       flip = TRUE)
#' 
weekly_frequency_plot <- function(gw_level_dv, 
                                  gwl_data, 
                                  parameter_cd = NA,
                                  date_col = NA,
                                  value_col = NA, 
                                  approved_col = NA,
                                  stat_cd = NA,
                                  plot_range = "Past year",
                                  plot_title = "", 
                                  subtitle = "U.S. Geological Survey",
                                  y_axis_label = "",
                                  flip = FALSE) {
  
  data_list <- set_up_data(gw_level_dv = gw_level_dv, 
                           gwl_data = gwl_data, 
                           parameter_cd = parameter_cd,
                           date_col = date_col,
                           value_col = value_col, 
                           approved_col = approved_col,
                           stat_cd = stat_cd)
  
  gw_level_dv <- data_list$gw_level_dv
  gwl_data <- data_list$gwl_data
  
  plot_range <- match.arg(plot_range, choices = c("Past year",
                                                  "Calendar year"),
                          several.ok = FALSE)
  
  date <- max(c(gw_level_dv$Date,
                gwl_data$Date),
              na.rm = TRUE)
  
  # Calculate the percentiles
  site_statistics <- weekly_frequency_table(gw_level_dv, 
                                            gwl_data,
                                            date_col = c("Date", "Date"), 
                                            value_col = c("Value", "Value"),
                                            approved_col = c("Approve", "Approve"),
                                            flip = flip)

  # Find the bounds of the plot
  if(plot_range == "Past year") {
    plot_end <- last_day(date) + 1
    plot_start <- first_day(plot_end - 363)
  } else if(plot_range == "Calendar year") {
    calendar_year <- as.character(date, format = "%Y")
    plot_end <- as.Date(paste0(calendar_year, "-12-31"))
    plot_start <- as.Date(paste0(calendar_year, "-01-01"))
  }
  
  # The last year of groundwater level measurements will plot
  gw_level_plot <- gw_level_dv %>%
    dplyr::filter(Date >= plot_start)
  
  # Add the first day of the week to the site_statistics table for plotting
  plot_week <- seq(as.Date(plot_start), length = 52, by = "1 week")
  plot_week_lookup <- data.frame(plot_week = plot_week, 
                                 week = as.POSIXlt(plot_week)$yday%/%7 + 1)
  
  site_statistics <- dplyr::left_join(site_statistics,
                                      plot_week_lookup,
                                      by = "week")
  
  # Set up the plot data for the percentile ranges (rectangle geometry)
  site_statistics_pivot <- site_statistics %>%
    dplyr::select(-week, -nYears, -minMed, -maxMed, -week_start) %>%
    tidyr::pivot_longer(cols = -plot_week, 
                        names_to = "name",
                        values_to = "value")
  
  cols <- list(c("p05", "p10"), c("p10", "p25"), c("p25", "p75"),
               c("p75", "p90"), c("p90", "p95"))
  groups <- c("5 - 10", "10 - 25", "25 - 75", "75 - 90", "90 - 95")
  plot_list <- data.frame()
  for(i in seq_along(cols)) {
    plot_data <- site_statistics_pivot %>%
      dplyr::filter(name %in% cols[[i]]) %>%
      tidyr::pivot_wider(id_cols = plot_week, names_from = name, values_from = value) %>%
      dplyr::rename(ymin = cols[[i]][1], ymax = cols[[i]][2]) %>%
      dplyr::mutate(group = groups[i])
    plot_list <- dplyr::bind_rows(plot_list, plot_data)
  }
  
  # Make the group an ordered factor so the legend has the correct order
  # and add the last day of the month to draw the rectangles
  site_statistics_plot <- plot_list %>%
    dplyr::mutate(group = factor(group,
                          levels = groups,
                          ordered = TRUE),
           plot_week_last = plot_week + 7)
  
  # The median value will plot in the middle of the month
  site_statistics_med <- site_statistics %>%
    dplyr::mutate(plot_week_med = plot_week + 3,
           group = "Historical weekly median") %>%
    dplyr::select(plot_week_med, p50, group) %>%
    dplyr::rename(x = plot_week_med, y = p50)
  
  data_points <- gw_level_plot %>%
    dplyr::mutate(gw_code = ifelse(grepl("A", Approve), "Approved", "Provisional"),
           group = sprintf("%s daily value", gw_code)) %>%
    dplyr::rename(x = Date,
           y = Value) %>%
    dplyr::select(x, y, group)
  
  point_data <- dplyr::bind_rows(site_statistics_med, data_points) %>%
    dplyr::mutate(group = factor(group,
                          levels = c("Historical weekly median",
                                     "Approved daily value",
                                     "Provisional daily value"),
                          ordered = TRUE))
  
  # Assign colors and shapes
  rectangle_colors <- c("5 - 10" = "palevioletred2",
                        "10 - 25" = "rosybrown1",
                        "25 - 75" = "darkolivegreen1",
                        "75 - 90" = "lightskyblue2",
                        "90 - 95" = "skyblue3")
  point_shapes <- c("Historical weekly median" = 17,
                    "Provisional daily value" = 16,
                    "Approved daily value" = 16)
  point_colors <- c("Approved daily value" = "black",
                    "Provisional daily value" = "red",
                    "Historical weekly median" = "springgreen4")
  
  point_shapes <- point_shapes[as.character(unique(point_data$group))]
  point_colors <- point_colors[as.character(unique(point_data$group))]
  
  # Create the plot labels
  year_start <- as.POSIXlt(plot_start)$year + 1900
  year_end <- as.POSIXlt(plot_end)$year + 1900
  
  if(year_start == year_end) {
    x_label <- as.character(year_start)
  } else {
    x_label <- paste(year_start, year_end, sep = " - ")
  }
  y_label <- y_axis_label
  
  # Create the month breaks
  month_start <- seq(as.Date(plot_start), length = 12, by = "1 month")
  month_breaks <- mid_month(month_start)
  month_labels <- month.abb[as.POSIXlt(month_breaks)$mon + 1]

  order_groups <- c("Approved daily value",
                    "Provisional daily value",
                    "Historical weekly median")
  
  point_data$group <- factor(point_data$group, 
                             levels = order_groups)
  point_data$group <- droplevels(point_data$group)
  # Plot
  plot_out <- ggplot() +
    geom_rect(data = site_statistics_plot,
              aes(xmin = plot_week,
                  xmax = plot_week_last,
                  ymin = ymin, 
                  ymax = ymax, 
                  fill = group)) +
    geom_vline(xintercept = plot_week, color = "gray90") +
    geom_point(data = dplyr::filter(point_data, 
                                    group == "Historical weekly median"),
               aes(x = x, y = y, color = group),
               size = 1, shape = 17) +
    geom_line(data = dplyr::filter(point_data, 
                                   group != "Historical weekly median"),
               aes(x = x, y = y, color = group), size = 1) +
    geom_vline(xintercept = month_start, color = "grey70") +
    scale_color_manual(values = point_colors, breaks = order_groups,
                       name = "EXPLANATION") +
    scale_shape_manual(values = c(17, NA, NA), name = "EXPLANATION") +
    scale_fill_manual(values = rectangle_colors,
                      breaks = c("90 - 95",
                                 "75 - 90",
                                 "25 - 75",
                                 "10 - 25",
                                 "5 - 10"),
                      name = "Percentile") +
    scale_x_date(limits = c(plot_start, plot_end + 1), expand = c(0,0),
                 breaks = month_breaks, labels = month_labels) +
    hasp_framework(x_label, y_label, 
                   plot_title = plot_title,
                   subtitle = subtitle) +
    theme(axis.ticks.x = element_blank(),
          aspect.ratio = NULL)
  
  if(length(unique(point_data$group)) == 3){
    plot_out <- plot_out +
      guides(color = guide_legend(order = 1,
                                override.aes = list(shape = c(NA, NA, 17),
                                                    linetype = c("solid", "solid", "blank"))),
           shape = "none",
           fill = guide_legend(order = 2)) 
  } else if (length(unique(point_data$group)) == 2) {
    #TODO: be smarter:
    plot_out <- plot_out +
      guides(color = guide_legend(order = 1,
                                  override.aes = list(shape = c(NA, 17),
                                                      linetype = c("solid",  "blank"))),
             shape = "none",
             fill = guide_legend(order = 2))     
  }
  
  if(flip){
    plot_out <- plot_out +
      scale_y_continuous(trans = "reverse")
  }
  
  return(plot_out)
  
}

#' Plot recent data
#'
#' @param gw_level_dv data frame daily groundwater level data
#' from \code{readNWISdv}
#' @param parameter_cd If data in gw_level_dv comes from NWIS, the parameter_cd 
#' can be used to define the value_col.
#'  If the data doesn't come directly from NWIS services, this 
#' can be set to \code{NA},and this argument will be ignored.
#' @param date_col the heading of the date column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param value_col name of value column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param approved_col name of column to get provisional/approved status.
#' @param stat_cd If data in gw_level_dv comes from NWIS, the stat_cd 
#' can be used to help define the value_col.
#' @param start_date Date to start plot. If \code{NA} (which is the default),
#' the plot will start 2 years before the most recent value.
#' @param end_data Date to end plot. If \code{NA} (which is the default), 
#' the plot will end with the latest measurement. 
#' @param historical_stat the summary statistic to use for middle line of the plot. Either
#' "mean" or "median." 
#' @param month_breaks a logical indicating whether to use monthly breaks for the plot
#' @param plot_title the title to use on the plot
#' @param subtitle character. Sub-title for plot, default is "U.S. Geological Survey".
#' @param y_axis_label the label to use for the y axis
#' @param flip logical. If \code{TRUE}, flips the y axis so that the smallest number is on top.
#' Default is \code{FALSE}. 
#' @return a ggplot object with a ribbon indicating the historical daily range,
#' the historical daily mean or median, and approved and provisional
#' daily data for the last two years
#' 
#' @export
#' 
#' @import ggplot2
#'
#' @examples
#' 
#' site <- "263819081585801"
#' p_code_dv <- "62610"
#' statCd <- "00001"
#' # gw_level_dv <- dataRetrieval::readNWISdv(site, p_code_dv, statCd = statCd)
#' gw_level_dv <- L2701_example_data$Daily
#' daily_gwl_plot(gw_level_dv,
#'                parameter_cd = "62610",
#'                plot_title = "Groundwater Level", 
#'                month_breaks = TRUE,
#'                historical_stat = "median")
#'                
#' daily_gwl_plot(gw_level_dv,
#'                parameter_cd = "62610",
#'                plot_title = "Groundwater Level", 
#'                month_breaks = TRUE,
#'                start_date = "2020-10-01",
#'                historical_stat = "median")
#'                
#' daily_gwl_plot(gw_level_dv,
#'                parameter_cd = "62610",
#'                plot_title = "Groundwater Level", 
#'                month_breaks = TRUE,
#'                start_date = "2018-10-01",
#'                end_date = "2020-10-01",
#'                historical_stat = "median")
#' 
daily_gwl_plot <- function(gw_level_dv, 
                           parameter_cd = NA,
                           date_col = NA,
                           value_col = NA, 
                           approved_col = NA,
                           stat_cd = NA,
                           start_date = NA,
                           end_date = NA,
                           historical_stat = "mean", 
                           month_breaks = FALSE,
                           plot_title = "",
                           subtitle = "U.S. Geological Survey",
                           y_axis_label = "",
                           flip = FALSE) {

  data_list <- set_up_data(gw_level_dv = gw_level_dv, 
                           gwl_data = NULL, 
                           parameter_cd = parameter_cd,
                           date_col = date_col,
                           value_col = value_col, 
                           approved_col = approved_col,
                           stat_cd = stat_cd)
  
  gw_level_dv <- data_list$gw_level_dv
  gwl_data <- data_list$gwl_data
  
  gw_level_dv <- dplyr::bind_rows(gw_level_dv,
                                  gwl_data)
  
  historical_stat <- match.arg(historical_stat, 
                               choices = c("mean", "median"), 
                               several.ok = FALSE)
  historical_function <- switch(historical_stat, 
                                "median" = median,
                                "mean" = mean)
  historical_name <- paste("Historical", historical_stat)
  
  # Calculate the historical max/min/median for each day

  gw_level_dv$J = as.numeric(as.character(gw_level_dv$Date,
                                          format = "%j"))

  historical_stats <- gw_level_dv[grepl("A", gw_level_dv$Approve), ] %>%
    dplyr::group_by(J) %>%
    dplyr::summarize(max = max(Value, na.rm = TRUE),
                     middle = historical_function(Value, na.rm = TRUE),
                     min = min(Value, na.rm = TRUE))
  
  # Pull the last two years of data & join with the historical data
  
  if(is.na(end_date)){
    end_date <- max(c(gw_level_dv$Date), na.rm = TRUE)
  } else {
    end_date <- as.Date(end_date)
  }
  
  if(is.na(start_date)){
    plot_start_year <- as.numeric(as.character(end_date, format = "%Y")) - 2
    plot_start <- as.Date(paste(plot_start_year,
                                as.character(end_date, format = "%m-%d"),
                                sep = "-"))
  } else {
    plot_start <- as.Date(start_date)
  }

  
  # add a 10 day buffer following the most recent value
  plot_end <- end_date + as.difftime(10, units = "days")
  buffer_dates <- seq.Date(plot_start, plot_end, by = "day")[-1]
  buffer_dates <- buffer_dates[buffer_dates <= Sys.Date()]
  buffer_j <- as.numeric(as.character(buffer_dates, "%j"))
  buffer <- stats::setNames(data.frame(buffer_dates, buffer_j),
                            c("Date", "J"))

  plot_data <- gw_level_dv %>%
    dplyr::right_join(buffer, by = c("Date", "J")) %>%
    dplyr::left_join(historical_stats, by = "J") %>%
    dplyr::mutate(group = "Approved Daily\nMin & Max")
  
  line_data <- plot_data %>%
    dplyr::select(Date, Approve, Value, middle) %>%
    tidyr::pivot_longer(-Date:-Approve) %>%
    dplyr::mutate(group = ifelse(name == "Value",
                          ifelse(Approve == "A", "Approved daily value", "Provisional daily value"),
                          historical_name)) %>%
    dplyr::select(-Approve, -name) %>%
    dplyr::filter(!is.na(value))
  
  line_data$group <- ordered(line_data$group, 
                             levels = c("Approved daily value", 
                                        "Provisional daily value",
                                        historical_name))
  
  # Create the plot
  
  line_colors <- c("limegreen",
                   "Provisional daily value" = "red",
                   "Approved daily value" = "navy")
  names(line_colors)[1] <- historical_name
  ribbon_colors <- c("Approved Daily\nMin & Max" = "lightskyblue1")
  
  if(month_breaks) {
    x_label <- paste(as.character(plot_start, "%B %Y"), 
                     "to", 
                     as.character(plot_end, "%B %Y"))
    x_breaks <- mid_month(seq.Date(plot_start, plot_end, by = "month"))
    x_tick_labels <- substr(as.character(x_breaks, format = "%B"), 1, 1)
  } else {
    x_label <- "Date"
    x_breaks <- seq.Date(plot_start, end_date, by = "year")
    x_tick_labels <- as.character(x_breaks, format = "%Y")
  }
  
  y_label <- y_axis_label
  
  plot_out <- ggplot() +
    geom_ribbon(data = plot_data, 
                aes(x = Date, ymin = min, ymax = max, fill = group)) +
    geom_line(data = line_data, 
              aes(x = Date, y = value, color = group)) +
    scale_color_manual(values = line_colors, name = "EXPLANATION") +
    scale_fill_manual(values = ribbon_colors, name = "") +
    hasp_framework(x_label, y_label,
                   plot_title = plot_title,
                   subtitle = subtitle) +
    theme(aspect.ratio = NULL) +
    scale_x_date(limits = c(plot_start, plot_end), 
                 expand = c(0,0),
                 breaks = x_breaks, labels = x_tick_labels) +
    guides(color = guide_legend(order = 1),
           fill = guide_legend(order = 2)) 
  
  if(month_breaks) {
    plot_out <- plot_out +
      geom_vline(xintercept = seq.Date(plot_start, plot_end, by = "month"),
                 color = "grey80") +
      theme(axis.ticks.x = element_blank())
    
  } 
  
  if(flip){
    plot_out <- plot_out +
      scale_y_continuous(trans = "reverse")
  }
  
  return(plot_out)
  
}

#' Daily frequency table
#' 
#' Give the historical max, mean, minimum, and number of available points
#' for each day of the year
#' 
#' @param gw_level_dv data frame, daily groundwater level data
#' from \code{readNWISdv}
#' @param gwl_data data frame returned from dataRetrieval::readNWISgwl, or 
#' data frame with mandatory columns lev_dt (representing date), lev_age_cd (representing
#' approval code), and a column representing the measured value (either lev_va,
#' sl_lev_va, or value).
#' @param parameter_cd If data in gw_level_dv comes from NWIS, the parameter_cd 
#' can be used to define the value_col.
#'  If the data doesn't come directly from NWIS services, this 
#' can be set to \code{NA},and this argument will be ignored.
#' @param date_col the heading of the date column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param value_col name of value column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param stat_cd If data in gw_level_dv comes from NWIS, the stat_cd 
#' can be used to help define the value_col.
#' @param approved_col name of column to get provisional/approved status.
#' 
#' @return a data frame giving the max, mean, min, and number of available
#' days of data for each day of the year.
#' 
#' @export
#' 
#' @examples 
#' 
#' # site <- "263819081585801"
#' p_code_dv <- "62610"
#' statCd <- "00001"
#' # gw_level_dv <- dataRetrieval::readNWISdv(site, p_code_dv, statCd = statCd)
#' gw_level_dv <- L2701_example_data$Daily
#' daily_frequency_table(gw_level_dv,
#'                       NULL,
#'                       parameter_cd = "62610")
#' 
#' gwl_data <- L2701_example_data$Discrete
#' daily_frequency_table(gw_level_dv,
#'                       gwl_data,
#'                       parameter_cd = "62610")
daily_frequency_table <- function(gw_level_dv,
                                  gwl_data,
                                  parameter_cd = NA, 
                                  date_col = NA, value_col = NA, 
                                  approved_col = NA, stat_cd = NA) {
  
  data_list <- set_up_data(gw_level_dv = gw_level_dv, 
                           gwl_data = gwl_data, 
                           parameter_cd = parameter_cd,
                           date_col = date_col,
                           value_col = value_col, 
                           approved_col = approved_col,
                           stat_cd = stat_cd)
  
  gw_level_dv <- data_list$gw_level_dv
  gwl_data <- data_list$gwl_data

  gw_level_dv <- dplyr::bind_rows(gw_level_dv,
                                  gwl_data)
  
  historical_stats <- gw_level_dv[grepl("A", gw_level_dv$Approve), ] 
  historical_stats$DOY <- as.numeric(as.character(historical_stats$Date, "%j")) 

  historical_stats <- historical_stats %>%
    dplyr::group_by(DOY) %>%
    dplyr::summarize(max = max(Value, na.rm = TRUE),
              mean = mean(Value, na.rm = TRUE),
              min = min(Value, na.rm = TRUE),
              points = dplyr::n())
  return(historical_stats)
  
}

#' Summary table of daily data
#' 
#' @param gw_level_dv daily groundwater level data
#' from readNWISdv
#' @param gwl_data data frame returned from dataRetrieval::readNWISgwl, or 
#' data frame with mandatory columns lev_dt (representing date), lev_age_cd (representing
#' approval code), and a column representing the measured value (either lev_va,
#' sl_lev_va, or value).
#' @param parameter_cd If data in gw_level_dv comes from NWIS, the parameter_cd 
#' can be used to define the value_col.
#'  If the data doesn't come directly from NWIS services, this 
#' can be set to \code{NA},and this argument will be ignored.
#' @param date_col the heading of the date column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param value_col name of value column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param stat_cd If data in gw_level_dv comes from NWIS, the stat_cd 
#' can be used to help define the value_col.
#' @param approved_col name of column to get provisional/approved status.
#' 
#' @return a summary table giving the period of record, completeness
#' and percentile values
#' 
#' @export
#' 
#' @examples
#' 
#' # site <- "263819081585801"
#' p_code_dv <- "62610"
#' statCd <- "00001"
#' # gw_level_dv <- dataRetrieval::readNWISdv(site, p_code_dv, statCd = statCd)
#' gw_level_dv <- L2701_example_data$Daily
#' daily_gwl_summary(gw_level_dv,
#'                   gwl_data = NULL,
#'                   parameter_cd = p_code_dv)
#' 
#' gwl_data <- L2701_example_data$Discrete
#' daily_gwl_summary(gw_level_dv,
#'                   gwl_data = gwl_data,
#'                   parameter_cd = p_code_dv)
daily_gwl_summary <- function(gw_level_dv, 
                              gwl_data,
                              parameter_cd = NA,
                              date_col = NA,
                              value_col = NA,
                              approved_col = NA,
                              stat_cd = NA) {
  
  data_list <- set_up_data(gw_level_dv = gw_level_dv, 
                           gwl_data = gwl_data, 
                           parameter_cd = parameter_cd,
                           date_col = date_col,
                           value_col = value_col, 
                           approved_col = approved_col,
                           stat_cd = stat_cd)
  
  gw_level_dv <- data_list$gw_level_dv
  gwl_data <- data_list$gwl_data
  
  gw_level_dv <- dplyr::bind_rows(gw_level_dv,
                                  gwl_data)
    
  gw_level_dv$gw_level <- gw_level_dv$Value
  gw_level_dv$gw_level_cd <- gw_level_dv$Approve
  
  gw_level_dv <- gw_level_dv[grepl("A", gw_level_dv$gw_level_cd), ]
  
  begin_date <- min(gw_level_dv$Date, na.rm = TRUE)
  end_date <- max(gw_level_dv$Date, na.rm = TRUE)
  days <- nrow(gw_level_dv)
  percent_complete <- round(days/length(seq.Date(begin_date, end_date, by = "day")) * 100, 0)
  lowest_level <- min(gw_level_dv$gw_level, na.rm = TRUE)
  highest_level <- max(gw_level_dv$gw_level, na.rm = TRUE)
  quant <- quantile(gw_level_dv$gw_level, type = 6,
                    probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.90, 0.95),
                    na.rm = TRUE)
  dv_summary <- data.frame(
    begin_date = begin_date,
    end_date = end_date,
    days = days,
    percent_complete = percent_complete,
    lowest_level = lowest_level,
    p5 = quant[1],
    p10 = quant[2],
    p25 = quant[3],
    p50 = quant[4],
    p75 = quant[5],
    p90 = quant[6],
    p95 = quant[7],
    highest_level = highest_level,
    row.names = NULL)
  
  return(dv_summary)
  
}

#' Find the first day of the month for a given date
#' 
#' @param date a vector of dates
#' 
#' @return the first day of the month that given dates fall in
#' @export
#' 
#' @examples 
#' date <- as.Date("2020-12-28")
#' first_day(date)
#' 
first_day <- function(date) {
  
  date <- as.POSIXlt(date)
  
  first_day_month <- as.Date(paste(
    date$year + 1900,
    date$mon + 1,
    1,
    sep = "-"
  ))
  
  return(first_day_month)
  
}

#' Find the last day of the month for a given date
#' 
#' @param date a vector of dates
#' 
#' @return the last day of the month that given dates fall in
#' @export
#' @examples 
#' date <- as.Date("2020-12-28")
#' last_day(date)
#' last_day("2020-02-15")
#' last_day("2019-02-15")
#' last_day(c("2020-12-28", "2020-02-15", "2019-02-15"))
last_day <- function(date) {
  
  date <- as.POSIXlt(date)
  
  year <- date$year + 1900
  month <- date$mon + 1
  
  is_leap <- as.numeric((year %% 4 == 0 & year %% 100 != 0) | 
                          year %% 400 == 0)
  
  total_day <- c(31, 28,
                 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  last_day_month <- as.Date(paste(
    year,
    month,
    total_day[month],
    sep = "-"
  ))
  
  if(any(is_leap & month == 2)){
    last_day_month[is_leap & month == 2] <- as.Date(paste(year[is_leap & month == 2],
                                                          "02-29",
                                                          sep = "-"))
  }
  
  
  return(last_day_month)
  
}


#' Find the middle of the month for a given date
#' 
#' @param date a vector of dates
#' 
#' @return the middle day of the month the given dates fall in
#' @export
#' @examples 
#' date <- as.Date("2020-12-28")
#' mid_month(date)
#' mid_month(c("2019-02-15", "2020-03-08", "2010-06-01"))
mid_month <- function(date) {
  
  last_days <- last_day(date)
  first_days <- first_day(date)
  
  mid <- first_days + difftime(last_days, first_days)/2

  return(mid)
  
}
