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