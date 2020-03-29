#' Create a table of monthly frequency analysis
#' 
#' @param gw_level_data groundwater level data from \code{readNWISgwl}
#' 
#' @return a data frame of monthly groundwater level statistics including the
#' 5th, 10th, 25th, 75th, 90th, and 95th percentiles; the number of
#' years of data; and the lowest monthly median and the highest monthly
#' median.
#' 
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate week
#' @import dplyr
#' 
#' @export
#' 
#' @examples 
#' 
#' site <- "263819081585801"
#' gw_level_data <- dataRetrieval::readNWISgwl(site)
#' monthly_frequency <- monthly_frequency_table(gw_level_data)

monthly_frequency_table <- function(gw_level_data) {
  
  year <- lev_dt <- month <- week <- sl_lev_va <- ".dplyr"
  
  if(!all(c("lev_dt", "sl_lev_va") %in% names(gw_level_data))) {
    stop("gw_level_data should include 'sl_lev_va' and 'lev_dt' columns")
  }
  
  datums <- unique(gw_level_data$sl_datum_cd)
  if(length(datums) > 1) {
    datums <- paste(datums, collapse = ", ")
    datums <- sprintf("(%s)", datums)
    stop(paste("Data has measurements using multiple vertical datums", datums))
  }
  
  gw_level_data <- gw_level_data %>%
    mutate(year = year(lev_dt),
           month = month(lev_dt),
           week = week(lev_dt))
  
  annual_stats <- gw_level_data %>%
    group_by(year, month) %>%
    summarize(median = median(sl_lev_va)) %>%
    group_by(month) %>%
    summarize(minMed = min(median, na.rm = TRUE),
              maxMed = max(median, na.rm = TRUE))
  
  monthly_stats <- gw_level_data %>%
    group_by(month) %>%
    summarize(p5 = quantile(sl_lev_va, probs=0.05, na.rm=TRUE),
              p10 = quantile(sl_lev_va, probs=0.1, na.rm=TRUE),
              p25 = quantile(sl_lev_va, probs=0.25, na.rm=TRUE),
              p50 = quantile(sl_lev_va, probs=0.5, na.rm=TRUE),
              p75 = quantile(sl_lev_va, probs=0.75, na.rm=TRUE),
              p90 = quantile(sl_lev_va, probs=0.9, na.rm=TRUE),
              p95 = quantile(sl_lev_va, probs=0.95,na.rm=TRUE),
              nYears = length(unique(year))) %>%
    left_join(annual_stats, by = "month")
  
  return(monthly_stats)
  
}

#' Plot monthly frequency analysis
#' 
#' @param gw_level_data groundwater level data from \code{readNWISgwl}
#' @param title the title to use on the plot
#' @param range the time frame to use for the plot. Either "Past year" to use the
#' last year of data, or "Calendar year" to use the current calendar year, beginning
#' in January.
#' 
#' @return a ggplot with rectangles representing the historical monthly percentile,
#' black triangles representing the hisotorical monthly median, and red diamonds
#' showing the last year of groundwater level measurements.
#' 
#' @import ggplot2
#' @import tidyr
#' @import dplyr
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom purrr reduce
#' 
#' @export
#'
#' @examples
#' 
#' site <- "263819081585801"
#' gwl_data <- dataRetrieval::readNWISgwl(site)
#' monthly_frequency_plot(gwl_data, title = "Groundwater level")
monthly_frequency_plot <- function(gw_level_data, 
                                   title = "",
                                   range = c("Past year",
                                             "Calendar year")) {
  
  lev_dt <- nYears <- minMed <- maxMed <- name <- value <- group <- 
    plot_month_med <- p50 <- sl_lev_va <- plot_month_last <- ymin <-
    ymax <- x <- y <- ".dplyr"
  
  range <- match.arg(range)
  
  date <- Sys.Date()
  
  # Calculate the percentiles
  site_statistics <- monthly_frequency_table(gw_level_data)
  
  # Find the bounds of the plot.
  if(range == "Past year") {
    plot_end <- last_day(date) + 1
    plot_start <- first_day(plot_end - 363)
  } else if(range == "Calendar year") {
    calendar_year <- as.character(date, format = "%Y")
    plot_end <- as.Date(paste0(calendar_year, "-12-31"))
    plot_start <- as.Date(paste0(calendar_year, "-01-01"))
  }
  
  # The last year of groundwater level measurements will plot
  gw_level_data <- filter(gw_level_data,
                          lev_dt >= plot_start,
                          lev_dt <= plot_end)
  
  # Add the first day of the month to the site_statistics table for plotting
  plot_month <- seq(as.Date(plot_start), length = 12, by = "1 month")
  plot_month_lookup <- data.frame(plot_month = plot_month, month = month(plot_month))
  site_statistics <- left_join(site_statistics, plot_month_lookup, by = "month")
  
  # Set up the plot data for the percentile ranges (rectangle geometry)
  site_statistics_pivot <- site_statistics %>%
    select(-month, -nYears, -minMed, -maxMed) %>%
    pivot_longer(cols = -plot_month, names_to = "name", values_to = "value")
  
  cols <- list(c("p5", "p10"), c("p10", "p25"), c("p25", "p75"),
               c("p75", "p90"), c("p90", "p95"))
  groups <- c("5 - 10", "10 - 25", "25 - 75", "75 - 90", "90 - 95")
  plot_list <- list()
  for(i in seq_along(cols)) {
    plot_list[[i]] <- site_statistics_pivot %>%
      filter(name %in% cols[[i]]) %>%
      pivot_wider(id_cols = plot_month, names_from = name, values_from = value) %>%
      rename(ymin = cols[[i]][1], ymax = cols[[i]][2]) %>%
      mutate(group = groups[i])
  }
  
  # Make the group an ordered factor so the legend has the correct order
  # and add the last day of the month to draw the rectangles
  site_statistics_plot <- reduce(plot_list, bind_rows) %>%
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
  
  points_plot <- gw_level_data %>%
    mutate(group = "Data point") %>%
    select(lev_dt, sl_lev_va, group) %>%
    rename(x = lev_dt, y = sl_lev_va) %>%
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
  if(year(plot_start) == year(plot_end)) {
    x_label <- as.character(year(plot_start))
  } else {
    x_label <- paste(year(plot_start), year(plot_end), sep = " - ")
  }
  datum <- unique(gw_level_data$sl_datum_cd)
  y_label <- sprintf("Groundwater level above %s, in feet", datum)
  
  # Plot
  plot <- ggplot() +
    geom_rect(data = site_statistics_plot,
              aes(xmin = plot_month,
                  xmax = plot_month_last,
                  ymin = ymin, 
                  ymax = ymax, 
                  fill = group),
              color = "black") +
    geom_point(data = points_plot,
               aes(x = x,
                   y = y,
                   shape = group,
                   color = group),
               size = 2.5) +
    geom_vline(xintercept = plot_month) +
    scale_color_manual(values = point_colors, name = NULL) +
    scale_shape_manual(values = point_shapes, name = NULL) +
    scale_fill_manual(values = rectangle_colors,
                      name = "Percentile") +
    scale_x_date(limits = c(plot_start, plot_end + 1), expand = c(0,0),
                 breaks = mid_month(plot_month),
                 labels = month.abb[month(plot_month)]) +
    ylab(y_label) + xlab(x_label) + 
    ggtitle(title, subtitle = "U.S. Geological Survey") +
    theme_gwl() +
    theme(axis.ticks.x = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical")

  return(plot)
}

#' Create a table of weekly frequency analysis
#' 
#' The weekly frequency analysis is based on daily values
#' 
#' @param gw_level_dv daily groundwater level data
#' from readNWISdv
#' @param parameterCd the parameter code used
#' @param statCd the statistic code used
#' 
#' @return a data frame of weekly frequency analysis
#' 
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate week
#' @import dplyr
#' 
#' @export
#' 
#' @examples 
#' 
#' site <- "263819081585801"
#' parameterCd <- "62610"
#' statCd <- "00001"
#' gw_level_dv <- dataRetrieval::readNWISdv(site, parameterCd, statCd = statCd)
#' weekly_frequency <- weekly_frequency_table(gw_level_dv, parameterCd, statCd)
#' 

weekly_frequency_table <- function(gw_level_dv, parameterCd, statCd) {
  
  Date <- gw_level <- ".dplyr"
  
  dv_heading <- sprintf("X_%s_%s", parameterCd, statCd)
  dv_heading_cd <- paste0(dv_heading, "_cd")
  
  if(!all(c(dv_heading, dv_heading_cd, "Date") %in% names(gw_level_dv))) {
    stop(sprintf("expected columns %s, %s, and Date in gw_level_dv", 
                 dv_heading, dv_heading_cd))
  }
  
  gw_level_dv <- gw_level_dv %>%
    mutate(year = year(Date),
           week = week(Date)) %>%
    rename(gw_level = dv_heading)
  
  annual_stats <- gw_level_dv %>%
    group_by(year, week) %>%
    summarize(median = median(gw_level)) %>%
    group_by(week) %>%
    summarize(minMed = min(median, na.rm = TRUE),
              maxMed = max(median, na.rm = TRUE))
  
  weekly_stats <- gw_level_dv %>%
    group_by(week) %>%
    summarize(p5 = quantile(gw_level, probs=0.05, na.rm=TRUE),
              p10 = quantile(gw_level, probs=0.1, na.rm=TRUE),
              p25 = quantile(gw_level, probs=0.25, na.rm=TRUE),
              p50 = quantile(gw_level, probs=0.5, na.rm=TRUE),
              p75 = quantile(gw_level, probs=0.75, na.rm=TRUE),
              p90 = quantile(gw_level, probs=0.9, na.rm=TRUE),
              p95 = quantile(gw_level, probs=0.95,na.rm=TRUE),
              nYears = length(unique(year))) %>%
    left_join(annual_stats, by = "week") %>%
    filter(week != 53)
  
  
}

#' Plot weekly frequency analysis
#' 
#' The weekly frequency analysis is based on daily data
#' 
#' @param gw_level_dv daily groundwater level data
#' from readNWISdv
#' @param parameterCd the parameter code used
#' @param statCd the statistic code used
#' @param title the title to use on the plot
#' @param range the time frame to use for the plot. Either "Past year" to use the
#' last year of data, or "Calendar year" to use the current calendar year, beginning
#' in January.
#' 
#' @return a ggplot object with rectangles representing the historical weekly percentiles,
#' and points representing the historical median and daily values
#' 
#' @import ggplot2
#' @import tidyr
#' @import dplyr
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom purrr reduce
#' 
#' @export
#'
#' @examples
#' 
#' site <- "263819081585801"
#' parameterCd <- "62610"
#' statCd <- "00001"
#' gw_level_dv <- dataRetrieval::readNWISdv(site, parameterCd, statCd = statCd)
#' weekly_frequency_plot(gw_level_dv, parameterCd, statCd, title = "Groundwater Level")
#' 

weekly_frequency_plot <- function(gw_level_dv, 
                                  parameterCd, 
                                  statCd,
                                  title = "", 
                                  range = c("Past year",
                                            "Calendar year")) {
  
  Date <- nYears <- minMed <- maxMed <- name <- value <- group <-
    plot_week_med <- p50 <- gw_code <- gw_level <- x <- y <- 
    plot_week_last <- ymin <- ymax <- ".dplyr"
  
  range <- match.arg(range)
  
  date <- Sys.Date()
  
  dv_heading <- sprintf("X_%s_%s", parameterCd, statCd)
  dv_heading_cd <- paste0(dv_heading, "_cd")
  
  # Calculate the percentiles
  site_statistics <- weekly_frequency_table(gw_level_dv, parameterCd, statCd)
  
  # Find the bounds of the plot
  if(range == "Past year") {
    plot_end <- last_day(date) + 1
    plot_start <- first_day(plot_end - 363)
  } else if(range == "Calendar year") {
    calendar_year <- as.character(date, format = "%Y")
    plot_end <- as.Date(paste0(calendar_year, "-12-31"))
    plot_start <- as.Date(paste0(calendar_year, "-01-01"))
  }
  
  # The last year of groundwater level measurements will plot
  gw_level_plot <- gw_level_dv %>%
    rename(gw_level = dv_heading,
           gw_code = dv_heading_cd) %>%
    filter(Date >= plot_start)
  
  # Add the first day of the week to the site_statistics table for plotting
  plot_week <- seq(as.Date(plot_start), length = 52, by = "1 week")
  plot_week_lookup <- data.frame(plot_week = plot_week, week = week(plot_week))
  site_statistics <- left_join(site_statistics, plot_week_lookup, by = "week")
  
  # Set up the plot data for the percentile ranges (rectangle geometry)
  site_statistics_pivot <- site_statistics %>%
    select(-week, -nYears, -minMed, -maxMed) %>%
    pivot_longer(cols = -plot_week, names_to = "name", values_to = "value")
  
  cols <- list(c("p5", "p10"), c("p10", "p25"), c("p25", "p75"),
               c("p75", "p90"), c("p90", "p95"))
  groups <- c("5 - 10", "10 - 25", "25 - 75", "75 - 90", "90 - 95")
  plot_list <- list()
  for(i in seq_along(cols)) {
    plot_list[[i]] <- site_statistics_pivot %>%
      filter(name %in% cols[[i]]) %>%
      pivot_wider(id_cols = plot_week, names_from = name, values_from = value) %>%
      rename(ymin = cols[[i]][1], ymax = cols[[i]][2]) %>%
      mutate(group = groups[i])
  }
  
  # Make the group an ordered factor so the legend has the correct order
  # and add the last day of the month to draw the rectangles
  site_statistics_plot <- reduce(plot_list, bind_rows) %>%
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
    mutate(gw_code = ifelse(grepl("A", gw_code), "Approved", "Provisional"),
           group = sprintf("%s daily value", gw_code)) %>%
    rename(x = Date, y = gw_level) %>%
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
  point_colors <- c("Historical weekly median" = "springgreen4",
                    "Provisional daily value" = "red",
                    "Approved daily value" = "black")

  
  # Create the plot labels
  if(year(plot_start) == year(plot_end)) {
    x_label <- as.character(year(plot_start))
  } else {
    x_label <- paste(year(plot_start), year(plot_end), sep = " - ")
  }
  y_label <- readNWISpCode(parameterCd)$parameter_nm
  
  # Create the month breaks
  month_start <- seq(as.Date(plot_start), length = 12, by = "1 month")
  month_breaks <- mid_month(month_start)
  month_labels <- month.abb[month(month_breaks)]
  
  # Plot
  plot <- ggplot() +
    geom_vline(xintercept = month_start, color = "grey70") +
    geom_rect(data = site_statistics_plot,
              aes(xmin = plot_week,
                  xmax = plot_week_last,
                  ymin = ymin, 
                  ymax = ymax, 
                  fill = group)) +
    geom_vline(xintercept = plot_week, color = "gray90") +
    geom_point(data = point_data,
               aes(x = x,
                   y = y,
                   shape = group,
                   color = group),
               size = 1) +
    scale_color_manual(values = point_colors, name = NULL) +
    scale_shape_manual(values = point_shapes, name = NULL) +
    scale_fill_manual(values = rectangle_colors,
                      name = "Percentile") +
    scale_x_date(limits = c(plot_start, plot_end + 1), expand = c(0,0),
                 breaks = month_breaks, labels = month_labels) +
    ylab(y_label) + xlab(x_label) + ggtitle(title) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.ticks.x = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical")
  
  return(plot)
  
}

#' Find the first day of the month for a given date
#' 
#' @param date a vector of dates
#' 
#' @return the first day of the month that given dates fall in
#' 
#' @importFrom lubridate year
#' @importFrom lubridate month

first_day <- function(date) {
  
  first_day_month <- as.Date(paste(
    year(date),
    month(date),
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
#' 
#' @importFrom lubridate days_in_month
#' @importFrom lubridate year
#' @importFrom lubridate month

last_day <- function(date) {
  
  last_day_month <- as.Date(paste(
    year(date),
    month(date),
    days_in_month(date),
    sep = "-"
  ))
  
  return(last_day_month)
  
}


#' Find the middle of the month for a given date
#' 
#' @param date a vector of dates
#' 
#' @return the middle day of the month the given dates fall in

mid_month <- function(date) {
  
  mid <- lapply(date, function(x) { mean(c(x, last_day(x))) })
  mid <- do.call(c, mid)
  
  return(mid)
  
}
