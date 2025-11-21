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
  
  if (any(is_leap & month == 2)) {
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
  
  mid <- first_days + difftime(last_days, first_days) / 2
  
  return(mid)
  
}


#' Get range 
#'
#' @param plot_range the time frame to use for the plot. Use "Past year" (default) to see the
#' last year of data, or "Calendar year" to use the current calendar year, beginning
#' in January. Or specify two dates representing the start and end of the plot.
#' @param full_range the full time range of the data.
#' 
#' @return list of plot_start, plot_end, label
#' @export
#' @examples
#' full_range <- c("2000-01-01", "2025-10-01")
#' get_range("Past year", full_range)
#' get_range("Calendar year", full_range)
#' get_range(c("2024-10-01", "2025-06-01"), full_range)
#' get_range(c(NA, "2020-01-01"), full_range)
#' get_range(c("2020-01-01", NA), full_range)
get_range <- function(plot_range, full_range){
  
  # Find the bounds of the plot
  if(length(plot_range) == 2){
    plot_end <- as.Date(plot_range[2])
    plot_start <- as.Date(plot_range[1])
  } else if (plot_range == "Past year") {
    plot_end <- last_day(Sys.Date()) + 1
    plot_start <- first_day(plot_end - 363)
  } else if (plot_range == "Calendar year") {
    calendar_year <- format(Sys.Date(), format = "%Y")
    plot_end <- as.Date(paste0(calendar_year, "-12-31"))
    plot_start <- as.Date(paste0(calendar_year, "-01-01"))
  } else {
    stop("plot_range must be 2 dates or 'Past year' or 'Calendar year'")
  }
  
  if(is.na(plot_start)){
    plot_start <- full_range[1]
  }
  
  if(is.na(plot_end)){
    plot_end <- full_range[2]
  }
  
  # Create the plot labels
  start_year <- as.POSIXlt(plot_start)$year + 1900
  end_year <- as.POSIXlt(plot_end)$year + 1900
  if (start_year == end_year) {
    x_label <- as.character(start_year)
  } else {
    x_label <- paste(start_year, end_year, sep = " - ")
  }
  return(list(plot_start = plot_start,
              plot_end = plot_end,
              x_label = x_label))
}
