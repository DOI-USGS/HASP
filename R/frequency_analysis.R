#' Create a table of monthly frequency analysis
#' 
#' @param gw_level_dv daily groundwater level data frame. Often obtained from from \code{readNWISdv}
#' 
#' @param parameter_cd If data in gw_level_dv comes from NWIS, the parameter_cd 
#' can be used to define the value_col.
#'  If the data doesn't come directly from NWIS services, this 
#' can be set to \code{NA},and this argument will be ignored.
#' @param date_col the heading of the date column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param value_col name of value column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param approved_col name of column to get provisional/approved status.
#' 
#' @return a data frame of monthly groundwater level statistics including the
#' 5th, 10th, 25th, 75th, 90th, and 95th percentiles; the number of
#' years of data; and the lowest monthly median and the highest monthly
#' median.
#' 
#' @import dplyr
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
#'                                              parameter_cd = "62610")
#' head(monthly_frequency)
monthly_frequency_table <- function(gw_level_dv, 
                                    parameter_cd = NA,
                                    date_col = NA,
                                    value_col = NA, approved_col = NA) {
  
  year <- lev_dt <- month <- week <- sl_lev_va <- ".dplyr"
  
  
  date_col <- ifelse(is.na(date_col), "Date", date_col)
  value_col <- get_value_column(parameter_cd, gw_level_dv, value_col)
  approved_col <- ifelse(is.na(approved_col),
                        paste0(value_col, "_cd"),
                        approved_col) 
  
  if(!all(c(date_col, value_col, approved_col) %in% names(gw_level_dv))) {
    stop("not all required columns found in gw_level_dv")
  }
  
  gw_level_dv <- gw_level_dv %>%
    filter(grepl("A", !!sym(approved_col))) %>%
    mutate(year = as.POSIXlt(!!sym(date_col))$year + 1900,
           month = as.POSIXlt(!!sym(date_col))$mon + 1,
           week = as.POSIXlt(!!sym(date_col))$yday%/%7 + 1)
  
  annual_stats <- gw_level_dv %>%
    group_by(year, month) %>%
    summarize(median = median(!!sym(value_col))) %>%
    group_by(month) %>%
    summarize(minMed = min(median, na.rm = TRUE),
              maxMed = max(median, na.rm = TRUE))
  
  monthly_stats <- gw_level_dv %>%
    group_by(month) %>%
    summarize(p5 = quantile(!!sym(value_col), probs=0.05, na.rm=TRUE),
              p10 = quantile(!!sym(value_col), probs=0.1, na.rm=TRUE),
              p25 = quantile(!!sym(value_col), probs=0.25, na.rm=TRUE),
              p50 = quantile(!!sym(value_col), probs=0.5, na.rm=TRUE),
              p75 = quantile(!!sym(value_col), probs=0.75, na.rm=TRUE),
              p90 = quantile(!!sym(value_col), probs=0.9, na.rm=TRUE),
              p95 = quantile(!!sym(value_col), probs=0.95,na.rm=TRUE),
              nYears = length(unique(year))) %>%
    left_join(annual_stats, by = "month")
  
  return(monthly_stats)
  
}

#' Plot monthly frequency analysis
#' 
#' @param gw_level_dv data frame, daily groundwater level data. Often obtained from \code{readNWISdv}
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
#' @param plot_title the title to use on the plot
#' @param y_axis_label the label used for the y-axis of the plot.
#' @param flip_y logical. If \code{TRUE}, flips the y axis so that the smallest number is on top.
#' Default is \code{TRUE}. 
#' @return a ggplot with rectangles representing the historical monthly percentile,
#' black triangles representing the historical monthly median, and red diamonds
#' showing the last year of groundwater level measurements.
#' 
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @import dplyr
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
#'                                             parameter_cd = "62610",
#'                                             plot_title = "L2701 Groundwater Level",
#'                                             y_axis_label = label,
#'                                             flip_y = FALSE)
#' monthly_frequency
#' 
#' monthly_frequency_flip <- monthly_frequency_plot(gw_level_dv,
#'                                              parameter_cd = "62610",
#'                                              y_axis_label = label,
#'                                              plot_title = "L2701 Groundwater Level", 
#'                                              flip_y = TRUE)
#' monthly_frequency_flip 
#' 
monthly_frequency_plot <- function(gw_level_dv, 
                                   parameter_cd = NA,
                                   date_col = NA,
                                   value_col = NA, approved_col = NA,
                                   plot_title = "",
                                   plot_range = c("Past year"),
                                   y_axis_label = "",
                                   flip_y = FALSE) {
  
  lev_dt <- nYears <- minMed <- maxMed <- name <- value <- group <- 
    plot_month_med <- p50 <- sl_lev_va <- plot_month_last <- ymin <-
    ymax <- x <- y <- year <- month <- ".dplyr"
  p5 <- p10 <- p25 <- p75 <- p90 <- p95 <- ".dplyr"
  
  date_col <- ifelse(is.na(date_col), "Date", date_col)
  value_col <- get_value_column(parameter_cd, gw_level_dv, value_col)
  approved_col <- ifelse(is.na(approved_col),
                        paste0(value_col, "_cd"),
                        approved_col)   
  
  
  plot_range <- match.arg(plot_range)
  
  date <- max(gw_level_dv[[date_col]], na.rm = TRUE)
  
  #Convert date column to date just in case its a POSIXct:
  gw_level_dv[[date_col]] <- as.Date(gw_level_dv[[date_col]])
  
  # Calculate the percentiles
  site_statistics <- monthly_frequency_table(gw_level_dv, 
                                             date_col = date_col,
                                             value_col = value_col,
                                             approved_col = approved_col)
  
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
  gw_level_dv <- filter(gw_level_dv,
                     !!sym(date_col) >= plot_start,
                     !!sym(date_col) <= plot_end)
  
  # Add the first day of the month to the site_statistics table for plotting
  plot_month <- seq(as.Date(plot_start), length = 12, by = "1 month")
  plot_month_lookup <- data.frame(plot_month = plot_month, 
                                  month = as.POSIXlt(plot_month)$mon + 1)
  site_statistics <- left_join(site_statistics, plot_month_lookup, by = "month")
  
  if(flip_y){
    site_statistics <- site_statistics %>% 
      rename(p95_new = p5, p90_new = p10, p75_new = p25, 
             p25_new = p75, p10_new = p90, p5_new = p95)
    names(site_statistics) <- gsub("_new", "", names(site_statistics))
  }
  
  # Set up the plot data for the percentile ranges (rectangle geometry)
  site_statistics_pivot <- site_statistics %>%
    select(-month, -nYears, -minMed, -maxMed) %>%
    pivot_longer(cols = -plot_month, names_to = "name", values_to = "value")
  
  cols <- list(c("p5", "p10"), c("p10", "p25"), c("p25", "p75"),
               c("p75", "p90"), c("p90", "p95"))
  groups <- c("5 - 10", "10 - 25", "25 - 75", "75 - 90", "90 - 95")
  plot_list <- data.frame()
  for(i in seq_along(cols)) {
    plot_data <- site_statistics_pivot %>%
      filter(name %in% cols[[i]]) %>%
      pivot_wider(id_cols = plot_month, names_from = name, values_from = value) %>%
      rename(ymin = cols[[i]][1], ymax = cols[[i]][2]) %>%
      mutate(group = groups[i])
    
    plot_list <- bind_rows(plot_list, plot_data)
  }
  
  # Make the group an ordered factor so the legend has the correct order
  # and add the last day of the month to draw the rectangles
  site_statistics_plot <- plot_list %>%
    mutate(group = factor(group,
                          levels = groups,
                          ordered = TRUE),
           plot_month_last = last_day(plot_month) + 1)
  
  # The median value will plot in the middle of the month
  site_statistics_med <- site_statistics %>%
    mutate(plot_month_med = mid_month(plot_month),
           group = "Monthly median") %>%
    select(plot_month_med, p50, group) %>%
    rename(x = plot_month_med, y = p50)
  
  points_plot <- gw_level_dv %>%
    mutate(group = "Data point") %>%
    select(!!date_col, !!value_col, group) %>%
    rename(x = !!date_col, y = !!value_col) %>%
    bind_rows(site_statistics_med)
  
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
    geom_vline(xintercept = plot_month) +
    geom_point(data = points_plot,
               aes(x = x,
                   y = y,
                   shape = group,
                   color = group),
               size = 2.5) +
    scale_color_manual(values = point_colors, name = "EXPLANATION") +
    scale_shape_manual(values = point_shapes, name = "EXPLANATION") +
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
    hasp_framework(x_label, y_label, plot_title = plot_title) +
    theme(axis.ticks.x = element_blank()) +
    guides(color = guide_legend(order = 1, 
                                override.aes = list(shape = rev(point_shapes))),
           shape = "none",
           fill = guide_legend(order = 2))  
  if(flip_y){
    plot_out <- plot_out +
      scale_y_continuous(trans = "reverse")
  }
  return(plot_out)
}

#' Create a table of weekly frequency analysis
#' 
#' The weekly frequency analysis is based on daily values
#' 
#' @param gw_level_dv data frame, daily groundwater level data. Often obtained
#' from \code{readNWISdv}.
#' @param parameter_cd If data in gw_level_dv comes from NWIS, the parameter_cd 
#' can be used to define the value_col.
#'  If the data doesn't come directly from NWIS services, this 
#' can be set to \code{NA},and this argument will be ignored.
#' @param date_col the heading of the date column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param value_col name of value column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param approved_col name of column to get provisional/approved status.
#' 
#' @return a data frame of weekly frequency analysis
#' 
#' @import dplyr
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
#'                                            parameter_cd = "62610")
#' head(weekly_frequency)
weekly_frequency_table <- function(gw_level_dv, parameter_cd = NA,
                                   date_col = NA, value_col = NA, approved_col = NA) {
  
  Date <- gw_level <- year <- week <- ".dplyr"
  
  date_col <- ifelse(is.na(date_col), "Date", date_col)
  value_col <- get_value_column(parameter_cd, gw_level_dv, value_col)
  approved_col <- ifelse(is.na(approved_col),
                        paste0(value_col, "_cd"),
                        approved_col)   
  
  
  if(!all(c(date_col, value_col, approved_col) %in% names(gw_level_dv))) {
    stop("not all columns found in gw_level_dv")
  }
  
  gw_level_dv <- gw_level_dv %>%
    filter(grepl("A", !!sym(approved_col))) %>%
    mutate(year = as.POSIXlt(!!sym(date_col))$year + 1900,
           week = as.POSIXlt(!!sym(date_col))$yday%/%7 + 1 )
  
  annual_stats <- gw_level_dv %>%
    group_by(year, week) %>%
    summarize(median = median(!!sym(value_col))) %>%
    group_by(week) %>%
    summarize(minMed = min(median, na.rm = TRUE),
              maxMed = max(median, na.rm = TRUE))
  
  weekly_stats <- gw_level_dv %>%
    group_by(week) %>%
    summarize(p5 = quantile(!!sym(value_col), probs=0.05, na.rm=TRUE),
              p10 = quantile(!!sym(value_col), probs=0.1, na.rm=TRUE),
              p25 = quantile(!!sym(value_col), probs=0.25, na.rm=TRUE),
              p50 = quantile(!!sym(value_col), probs=0.5, na.rm=TRUE),
              p75 = quantile(!!sym(value_col), probs=0.75, na.rm=TRUE),
              p90 = quantile(!!sym(value_col), probs=0.9, na.rm=TRUE),
              p95 = quantile(!!sym(value_col), probs=0.95,na.rm=TRUE),
              nYears = length(unique(year))) %>%
    left_join(annual_stats, by = "week")
  
  return(weekly_stats)
  
}

#' Plot weekly frequency analysis
#' 
#' The weekly frequency analysis is based on daily data
#' 
#' @param gw_level_dv data frame, daily groundwater level data. Often obtained
#' from \code{readNWISdv}.
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
#' @param plot_title the title to use on the plot
#' @param y_axis_label the label used for the y-axis of the plot.
#' @param flip_y logical. If \code{TRUE}, flips the y axis so that the smallest number is on top.
#' Default is \code{FALSE}. 
#' @return a ggplot object with rectangles representing the historical weekly percentiles,
#' and points representing the historical median and daily values
#' 
#' @import ggplot2
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
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
#'                       date_col = "Date",
#'                       value_col = "X_62610_00001",
#'                       approved_col = "X_62610_00001_cd")
#'                       
#' weekly_frequency_plot(gw_level_dv, 
#'                       parameter_cd = "62610")
#'                       
#' weekly_frequency_plot(gw_level_dv, 
#'                       parameter_cd = "62610", 
#'                       flip_y = TRUE)
#' 
weekly_frequency_plot <- function(gw_level_dv, parameter_cd = NA,
                                  date_col = NA, value_col = NA,
                                  approved_col = NA,
                                  plot_range = c("Past year",
                                                 "Calendar year"),
                                  plot_title = "", 
                                  y_axis_label = "",
                                  flip_y = FALSE) {
  
  Date <- nYears <- minMed <- maxMed <- name <- value <- group <-
  plot_week_med <- p50 <- gw_code <- gw_level <- x <- y <- 
  plot_week_last <- ymin <- ymax <- week <- ".dplyr"
  p5 <- p10 <- p25 <- p75 <- p90 <- p95 <- ".dplyr"
  
  date_col <- ifelse(is.na(date_col), "Date", date_col)
  value_col <- get_value_column(parameter_cd, gw_level_dv, value_col)
  approved_col <- ifelse(is.na(approved_col),
                         paste0(value_col, "_cd"),
                         approved_col)   
  
  plot_range <- match.arg(plot_range)
  
  date <- max(gw_level_dv[[date_col]], na.rm = TRUE)
  
  # Calculate the percentiles
  site_statistics <- weekly_frequency_table(gw_level_dv, 
                                            date_col = date_col, 
                                            value_col = value_col,
                                            approved_col = approved_col)
  
  if(flip_y){
    site_statistics <- site_statistics %>% 
      rename(p95_new = p5, p90_new = p10, p75_new = p25, 
             p25_new = p75, p10_new = p90, p5_new = p95)
    names(site_statistics) <- gsub("_new", "", names(site_statistics))
  }
  
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
    filter(!!sym(date_col) >= plot_start)
  
  # Add the first day of the week to the site_statistics table for plotting
  plot_week <- seq(as.Date(plot_start), length = 52, by = "1 week")
  plot_week_lookup <- data.frame(plot_week = plot_week, 
                                 week = as.POSIXlt(plot_week)$yday%/%7 + 1)
  site_statistics <- left_join(site_statistics, plot_week_lookup, by = "week")
  
  # Set up the plot data for the percentile ranges (rectangle geometry)
  site_statistics_pivot <- site_statistics %>%
    select(-week, -nYears, -minMed, -maxMed) %>%
    pivot_longer(cols = -plot_week, names_to = "name", values_to = "value")
  
  cols <- list(c("p5", "p10"), c("p10", "p25"), c("p25", "p75"),
               c("p75", "p90"), c("p90", "p95"))
  groups <- c("5 - 10", "10 - 25", "25 - 75", "75 - 90", "90 - 95")
  plot_list <- data.frame()
  for(i in seq_along(cols)) {
    plot_data <- site_statistics_pivot %>%
      filter(name %in% cols[[i]]) %>%
      pivot_wider(id_cols = plot_week, names_from = name, values_from = value) %>%
      rename(ymin = cols[[i]][1], ymax = cols[[i]][2]) %>%
      mutate(group = groups[i])
    plot_list <- bind_rows(plot_list, plot_data)
  }
  
  # Make the group an ordered factor so the legend has the correct order
  # and add the last day of the month to draw the rectangles
  site_statistics_plot <- plot_list %>%
    mutate(group = factor(group,
                          levels = groups,
                          ordered = TRUE),
           plot_week_last = plot_week + 7)
  
  # The median value will plot in the middle of the month
  site_statistics_med <- site_statistics %>%
    mutate(plot_week_med = plot_week + 3,
           group = "Historical weekly median") %>%
    select(plot_week_med, p50, group) %>%
    rename(x = plot_week_med, y = p50)
  data_points <- gw_level_plot %>%
    mutate(gw_code = ifelse(grepl("A", !!sym(approved_col)), "Approved", "Provisional"),
           group = sprintf("%s daily value", gw_code)) %>%
    rename(x = Date,
           y = !!sym(value_col)) %>%
    select(x, y, group)
  
  point_data <- bind_rows(site_statistics_med, data_points) %>%
    mutate(group = factor(group,
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
  # Plot
  plot_out <- ggplot() +
    geom_rect(data = site_statistics_plot,
              aes(xmin = plot_week,
                  xmax = plot_week_last,
                  ymin = ymin, 
                  ymax = ymax, 
                  fill = group)) +
    geom_vline(xintercept = plot_week, color = "gray90") +
    geom_point(data = dplyr::filter(point_data, group == "Historical weekly median"),
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
    hasp_framework(x_label, y_label, plot_title = plot_title) +
    theme(axis.ticks.x = element_blank(),
          aspect.ratio = NULL)
  
  if(all(levels(point_data$group) %in% unique(point_data$group))){
    plot_out <- plot_out +
      guides(color = guide_legend(order = 1,
                                override.aes = list(shape = c(NA, NA, 17),
                                                    linetype = c("solid", "solid", "blank"))),
           shape = "none",
           fill = guide_legend(order = 2)) 
  } else {
    #TODO: be smarter:
    plot_out <- plot_out +
      guides(color = guide_legend(order = 1,
                                  override.aes = list(shape = c(NA, 17),
                                                      linetype = c("solid",  "blank"))),
             shape = "none",
             fill = guide_legend(order = 2))     
  }
  
  if(flip_y){
    plot_out <- plot_out +
      scale_y_continuous(trans = "reverse")
  }
  
  return(plot_out)
  
}

#' Plot the last two years of daily data
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
#' @param historical_stat the summary statstic to use for middle line of the plot. Either
#' "mean" or "median." 
#' @param month_breaks a logical indicating whether to use monthly breaks for the plot
#' @param plot_title the title to use on the plot
#' @param y_axis_label the label to use for the y axis
#' @param flip_y logical. If \code{TRUE}, flips the y axis so that the smallest number is on top.
#' Default is \code{FALSE}. 
#' @return a ggplot object with a ribbon indicating the historical daily range,
#' the historical daily mean or median, and approved and provisional
#' daily data for the last two years
#' 
#' @export
#' 
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom stats setNames
#'
#' @examples
#' 
#' site <- "263819081585801"
#' p_code_dv <- "62610"
#' statCd <- "00001"
#' # gw_level_dv <- dataRetrieval::readNWISdv(site, p_code_dv, statCd = statCd)
#' gw_level_dv <- L2701_example_data$Daily
#' daily_gwl_2yr_plot(gw_level_dv,
#'                    parameter_cd = "62610",
#'                    plot_title = "Groundwater Level", 
#'                    month_breaks = TRUE,
#'                    historical_stat = "median")
#' 
#' daily_gwl_2yr_plot(gw_level_dv,
#'                    date_col = "Date",
#'                    value_col = "X_62610_00001",
#'                    approved_col = "X_62610_00001_cd", 
#'                    plot_title = "Groundwater Level", 
#'                    month_breaks = TRUE,
#'                    historical_stat = "median",
#'                    flip_y = FALSE)
daily_gwl_2yr_plot <- function(gw_level_dv, parameter_cd = NA,
                               date_col = NA,
                               value_col = NA,
                               approved_col = NA,
                               historical_stat = c("mean", "median"), 
                               month_breaks = FALSE,
                               plot_title = "",
                               y_axis_label = "",
                               flip_y = FALSE) {
  
  Date <- gw_level_cd <- J <- gw_level <- name <- group <- value <- gw_level_cd <- middle <-
    ".dplyr"
  
  date_col <- ifelse(is.na(date_col), "Date", date_col)
  value_col <- get_value_column(parameter_cd, gw_level_dv, value_col)
  approved_col <- ifelse(is.na(approved_col),
                         paste0(value_col, "_cd"),
                         approved_col)   
  
  if(!all(c(date_col, value_col, approved_col) %in% names(gw_level_dv))) {
    stop("Not all required columns found in gw_level_dv")
  }
  
  historical_stat <- match.arg(historical_stat)
  historical_function <- switch(historical_stat, "median" = median, "mean" = mean)
  historical_name <- paste("Historical", historical_stat)
  
  # Calculate the historical max/min/median for each day
  
  gw_level_dv <- gw_level_dv %>%
    mutate(J = as.numeric(as.character(!!sym(date_col), format = "%j")))
  
  historical_stats <- gw_level_dv %>%
    filter(grepl("A", !!sym(approved_col))) %>%
    group_by(J) %>%
    summarize(max = max(!!sym(value_col), na.rm = TRUE),
              middle = historical_function(!!sym(value_col), na.rm = TRUE),
              min = min(!!sym(value_col), na.rm = TRUE))
  
  # Pull the last two years of data & join with the historical data
  
  most_recent <- max(gw_level_dv[, date_col], na.rm = TRUE)
  #TODO: check here!
  plot_start_year <- as.numeric(as.character(most_recent, format = "%Y")) - 2
  plot_start <- as.Date(paste0(plot_start_year, "-01-01"))
  
  # The plot has a ~3 month buffer following the most recent value
  plot_end <- most_recent + as.difftime(90, units = "days")
  buffer_dates <- seq.Date(most_recent, plot_end, by = "day")[-1]
  buffer_j <- as.numeric(as.character(buffer_dates, "%j"))
  buffer <- setNames(data.frame(buffer_dates, buffer_j), c(date_col, "J"))
  
  plot_data <- gw_level_dv %>%
    filter(!!sym(date_col) >= plot_start,
           !!sym(date_col) <= most_recent) %>%
    bind_rows(buffer) %>%
    left_join(historical_stats, by = "J") %>%
    mutate(group = "Approved Daily\nMin & Max")
  
  line_data <- plot_data %>%
    rename(Date = !!date_col, gw_level = !!value_col, gw_level_cd = !!approved_col) %>%
    select(Date, gw_level_cd, gw_level, middle) %>%
    pivot_longer(-Date:-gw_level_cd) %>%
    mutate(group = ifelse(name == "gw_level",
                          ifelse(gw_level_cd == "A", "Approved daily value", "Provisional daily value"),
                          historical_name)) %>%
    select(-gw_level_cd, -name) %>%
    filter(!is.na(value))
  
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
    x_breaks <- seq.Date(plot_start, most_recent, by = "year")
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
    hasp_framework(x_label, y_label, plot_title = plot_title) +
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
  
  if(flip_y){
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
#' @param parameter_cd If data in gw_level_dv comes from NWIS, the parameter_cd 
#' can be used to define the value_col.
#'  If the data doesn't come directly from NWIS services, this 
#' can be set to \code{NA},and this argument will be ignored.
#' @param date_col the heading of the date column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param value_col name of value column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param approved_col name of column to get provisional/approved status.
#' 
#' @return a data frame giving the max, mean, min, and number of available
#' days of data for each day of the year.
#' 
#' @import dplyr
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
#'                       parameter_cd = "62610")
#' 

daily_frequency_table <- function(gw_level_dv, parameter_cd = NA, 
                                  date_col = NA, value_col = NA, 
                                  approved_col = NA) {
  
  DOY <- ".dplyr"
  
  date_col <- ifelse(is.na(date_col), "Date", date_col)
  value_col <- get_value_column(parameter_cd, gw_level_dv, value_col)
  approved_col <- ifelse(is.na(approved_col),
                         paste0(value_col, "_cd"),
                         approved_col)  
  
  if(!all(c(date_col, value_col, approved_col) %in% names(gw_level_dv))) {
    stop("Not all columns found in gw_level_dv")
  }
  
  historical_stats <- gw_level_dv %>%
    filter(grepl("A", !!sym(approved_col))) %>%
    mutate(DOY = as.numeric(as.character(!!sym(date_col), "%j"))) %>%
    group_by(DOY) %>%
    summarize(max = max(!!sym(value_col), na.rm = TRUE),
              mean = mean(!!sym(value_col), na.rm = TRUE),
              min = min(!!sym(value_col), na.rm = TRUE),
              points = n())
  return(historical_stats)
  
}

#' Summary table of daily data
#' 
#' @param gw_level_dv daily groundwater level data
#' from readNWISdv
#' @param parameter_cd If data in gw_level_dv comes from NWIS, the parameter_cd 
#' can be used to define the value_col.
#'  If the data doesn't come directly from NWIS services, this 
#' can be set to \code{NA},and this argument will be ignored.
#' @param date_col the heading of the date column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param value_col name of value column. The default is \code{NA},
#' which the code will try to get the column name automatically.
#' @param approved_col name of column to get provisional/approved status.
#' 
#' @return a summary table giving the period of record, completeness
#' and percentile values
#' 
#' @export
#' 
#' @import dplyr
#'
#' @examples
#' 
#' # site <- "263819081585801"
#' p_code_dv <- "62610"
#' statCd <- "00001"
#' # gw_level_dv <- dataRetrieval::readNWISdv(site, p_code_dv, statCd = statCd)
#' gw_level_dv <- L2701_example_data$Daily
#' daily_gwl_summary(gw_level_dv,
#'                   parameter_cd = p_code_dv)
#' 
daily_gwl_summary <- function(gw_level_dv, parameter_cd = NA,
                              date_col = NA,
                              value_col = NA,
                              approved_col = NA) {
  
  gw_level <- gw_level_cd <- ".dplyr"

  date_col <- ifelse(is.na(date_col), "Date", date_col)
  value_col <- get_value_column(parameter_cd, gw_level_dv, value_col)
  approved_col <- ifelse(is.na(approved_col),
                        paste0(value_col, "_cd"),
                        approved_col)   

  
  if(!all(c(date_col, value_col, approved_col) %in% names(gw_level_dv))) {
    stop("Not all columns found in gw_level_dv")
  }
    
  gw_level_dv <- gw_level_dv %>%
    rename(gw_level = !!sym(value_col),
           gw_level_cd = !!sym(approved_col)) %>%
    filter(grepl("A", gw_level_cd))
  
  begin_date <- min(gw_level_dv[,date_col], na.rm = TRUE)
  end_date <- max(gw_level_dv[,date_col], na.rm = TRUE)
  days <- nrow(gw_level_dv)
  percent_complete <- round(days/length(seq.Date(begin_date, end_date, by = "day")) * 100, 0)
  lowest_level <- min(gw_level_dv$gw_level, na.rm = TRUE)
  highest_level <- max(gw_level_dv$gw_level, na.rm = TRUE)
  quant <- quantile(gw_level_dv$gw_level, 
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
last_day <- function(date) {
  
  date <- as.POSIXlt(date)
  
  year <- date$year + 1900
  month <- date$mon + 1
  
  is_leap <- as.numeric((year %% 4 == 0 & year %% 100 != 0) | 
                          year %% 400 == 0)
  
  total_day <- c(31, 28 + is_leap,
                 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  last_day_month <- as.Date(paste(
    year,
    month,
    total_day[month],
    sep = "-"
  ))
  
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
