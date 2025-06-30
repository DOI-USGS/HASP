context("Compute Statistical Tests")

test_that("Kendall Seasonal Trend", {
  
  gw_level_data <- L2701_example_data$Discrete
    
  less_data <- dplyr::filter(gw_level_data,
                             lev_dt > as.Date("2010-01-01"),
                             lev_dt < as.Date("2014-01-01"))
  
  test1 <- trend_test(NULL,
                     gwl_data = gw_level_data,
                     parameter_cd = "62610", 
                     days_required_per_month = 0)
  test2 <- expect_message(trend_test(NULL,
                                     less_data,
                                     parameter_cd = "62610", 
                                     days_required_per_month = 0))
  
  expect_true(all(c("test", "tau", "pValue", "slope", "intercept", "trend") %in% names(test1)))
  expect_true(all(c("test", "tau", "pValue", "slope", "intercept", "trend") %in% names(test2)))
  
  expect_true(all(is.na(c(test2$tau, test2$pValue, test2$slope, test2$intercept))))
  expect_true(-0.0229 == signif(test1$tau[2], digits = 3))
  expect_true(0.521 == signif(test1$pValue[2], digits = 3))
  expect_true(is.na(test1$slope[2]))
  expect_true(is.na(test1$intercept[2]))
  expect_true("Not significant" == test1$trend[2])
  
  #both:
  test1 <- trend_test(L2701_example_data$Daily,
                      gwl_data = gw_level_data,
                      parameter_cd = "62610", 
                      days_required_per_month = 0)
  expect_true(all(c("test", "tau", "pValue", "slope", "intercept", "trend") %in% names(test1)))
  expect_equal(test1$trend, c("Up", "Down"))
  #only dv:
  test1 <- trend_test(L2701_example_data$Daily,
                      gwl_data = NULL,
                      parameter_cd = "62610", 
                      days_required_per_month = 0)
  expect_true(all(c("test", "tau", "pValue", "slope", "intercept", "trend") %in% names(test1)))
  expect_equal(test1$trend, c("Up", "Down"))  
  
  test3 <- trend_test(gw_level_dv = NULL,
                      gwl_data =  gw_level_data,
                      parameter_cd = "62610")
  expect_true(-0.0229 == signif(test3$tau[2], digits = 3))
  
  qw_data <- L2701_example_data$QW
  test4 <- trend_test(NULL,
                      qw_data, 
                      approved_col = c(NA, "ResultStatusIdentifier"), 
                      date_col = c(NA, "ActivityStartDateTime"),
                      value_col = c(NA, "ResultMeasureValue"))
  
  expect_true(23.3 == signif(test4$slope[1], digits = 3))
  expect_true(2 == signif(test4$slope[2], digits = 3))
  
  test1 <- trend_test(L2701_example_data$Daily,
                      gwl_data = gw_level_data,
                      parameter_cd = "62610", 
                      POR_trend = FALSE,
                      days_required_per_month = 0)
  expect_true(all(c("test", "tau", "pValue", "slope", "intercept", "trend") %in% names(test1)))
  expect_equal(test1$trend, c("Up"))

  
})

test_that("Weekly frequency table", {
  
  wft <- weekly_frequency_table(L2701_example_data$Daily, 
                                NULL)
  expect_equal(as.numeric(head(wft$p25, 6)), c(-28.7, -29.1, -29.3, -29.7, -29.8, -29.1), tolerance = 0.05)
  expect_equal(as.numeric(head(wft$nYears, 6)), c(44, 45, 45, 45, 46, 46))
  expect_equal(tail(wft$minMed, 6), c(-42.2, -42.4, -42.8, -42.7, -42.5, -40.9), tolerance = 0.05)
  
  wft2 <- weekly_frequency_table(L2701_example_data$Daily, 
                                L2701_example_data$Discrete,
                                parameter_cd = "62610")
  
  expect_equal(as.numeric(head(wft2$p25, 6)), c(-28.7, -29.1, -29.3, -29.7, -29.8, -29.1), tolerance = 0.05)
  expect_equal(as.numeric(head(wft2$nYears, 6)), c(44, 45, 45, 45, 46, 46))
  expect_equal(tail(wft2$minMed, 6), c(-42.2, -42.4, -42.8, -42.7, -42.5, -40.9), tolerance = 0.05)
  
  wft2 <- weekly_frequency_table(L2701_example_data$Daily, 
                                 L2701_example_data$Discrete,
                                 parameter_cd = "62610",
                                 flip = TRUE)
  
  expect_equal(as.numeric(head(wft2$nYears, 6)), c(44, 45, 45, 45, 46, 46))
  expect_equal(tail(wft2$minMed, 6), 
               c(-4.91, -4.71, -4.89, -4.71, -4.86, -5.11), tolerance = 0.05)
  
})

test_that("Monthly frequency table", {
  
  mft <- monthly_frequency_table(L2701_example_data$Daily,
                                 NULL)
  
  expect_equal(as.numeric(head(mft$p75, 6)),
               c(-18.0575, -19.0500, -20.2625, -22.0500, -23.5100, -21.0200),
               tolerance = 0.05)
  expect_equal(as.numeric(head(mft$nYears, 6)),
               c(45, 46, 46, 46, 46, 45))
  expect_equal(tail(mft$maxMed, 6),
               c(-6.790, -4.820, -3.240, -4.800, -4.535, -4.860),
               tolerance = 0.05)
  
  mft2 <- monthly_frequency_table(L2701_example_data$Daily,
                                 L2701_example_data$Discrete,
                                 parameter_cd = "62610")
  
  expect_equal(as.numeric(head(mft2$p75, 6)),
               c(-18.0575, -19.0500, -20.2625, -22.0500, -23.5100, -21.0200),
               tolerance = 0.05)
  expect_equal(as.numeric(head(mft2$nYears, 6)),
               c(46, 46, 46, 46, 46, 45))
  expect_equal(tail(mft2$maxMed, 6),
               c(-6.790, -4.820, -3.240, -4.800, -4.535, -4.860),
               tolerance = 0.05)
  
  mft2 <- monthly_frequency_table(L2701_example_data$Daily,
                                  L2701_example_data$Discrete,
                                  parameter_cd = "62610",
                                  flip = TRUE)
  
  expect_equal(as.numeric(head(mft2$p75, 6)),
               c(-29.1325, -29.2400, -30.0975, -32.6600, -35.0900, -33.0800),
               tolerance = 0.05)
  expect_equal(as.numeric(head(mft2$nYears, 6)),
               c(46, 46, 46, 46, 46, 45))
  expect_equal(tail(mft2$maxMed, 6),
               c(-37.515, -32.360, -32.670, -36.155, -41.400, -42.450),
               tolerance = 0.05)
  
})

test_that("Daily summary table", {
  
  daily_summary_table <- daily_gwl_summary(L2701_example_data$Daily,
                                           NULL,
                                           parameter_cd = "62610")
  expect_equal(nrow(daily_summary_table), 1)
  expect_equal(daily_summary_table$percent_complete, 96)
  expect_equal(daily_summary_table$begin_date, as.Date("1978-10-01"))
  expect_equal(daily_summary_table$p25, -27.6, tolerance = 0.005)
  expect_equal(daily_summary_table$highest_level, -2.81, tolerance = 0.05)
  
  daily_summary_table2 <- daily_gwl_summary(L2701_example_data$Daily,
                                           L2701_example_data$Discrete,
                                           parameter_cd = "62610")
  expect_equal(nrow(daily_summary_table2), 1)
  expect_equal(daily_summary_table2$percent_complete, 99)
  expect_equal(daily_summary_table2$begin_date, as.Date("1978-10-01"))
  expect_equal(daily_summary_table2$p25, -27.8, tolerance = 0.005)
  expect_equal(daily_summary_table2$highest_level, -2.81, tolerance = 0.05)
  
})

test_that("Daily frequency table", {
  
  daily_frequency_table <- daily_frequency_table(L2701_example_data$Daily,
                                                 gwl_data = NULL)
  expect_equal(daily_frequency_table$max[1], -5.29, tolerance = 0.005)
  expect_equal(daily_frequency_table$min[365], -42.2, tolerance = 0.05)
  expect_equal(daily_frequency_table$mean[183], -24.3, tolerance = 0.05)
  
  daily_frequency_table2 <- daily_frequency_table(L2701_example_data$Daily,
                                                 gwl_data = L2701_example_data$Discrete,
                                                 parameter_cd = "62610")
  
  expect_equal(daily_frequency_table2$max[1], -5.29, tolerance = 0.005)
  expect_equal(daily_frequency_table2$min[365], -42.2, tolerance = 0.05)
  expect_equal(daily_frequency_table2$mean[183], -24.3, tolerance = 0.05)
  
})

test_that("trend segments",{
  
  gw_level_dv <- L2701_example_data$Daily
  qw_data <- L2701_example_data$QW
  
  trend_results <- trend_test(gw_level_dv = NULL,
                              gwl_data = qw_data,
                              parameter_cd = NA,
                              date_col = c(NA, "ActivityStartDateTime"),
                              value_col = c(NA, "ResultMeasureValue"), 
                              approved_col = c(NA, "ResultStatusIdentifier"),
                              stat_cd = NA)
  
  seg_df <- HASP:::create_segs(trend_results,
                               qw_data, 
                               value_col = "ResultMeasureValue",
                               date_col = "ActivityStartDateTime")
  
  expect_true(all(seg_df$x1 == as.Date(c("2011-04-29", "1978-09-16"))))
  expect_true(all(seg_df$x2 == as.Date(c("2021-04-27", "2021-04-27"))))
  expect_true(all(seg_df$years == c("10-year trend",
                                    "Period of record")))
  expect_true(all(seg_df$trend == c("Up", "Up")))
  
  trend_results <- trend_test(gw_level_dv = gw_level_dv,
                              gwl_data = NULL,
                              parameter_cd = "62610",
                              stat_cd = "00001")
  
  seg_df_dv <- HASP:::create_segs(trend_results,
                                  gw_level_dv, 
                                  date_col = "time", value_col = "value")
  
  expect_true(all(seg_df$x1 == c("2011-04-29", "1978-09-16")))
  expect_true(all(seg_df$x2 == c("2021-04-27", "2021-04-27")))
  
  expect_true(all(seg_df$years == c("10-year trend",
                                    "Period of record")))
  expect_true(all(seg_df$trend == c("Up", "Up")))
  
  expect_true(all(signif(seg_df_dv$y1, digits = 3) == c(-24.1, -18.1)))
  expect_true(all(signif(seg_df_dv$y2, digits = 3) == c(-11.6, -24.6)))
  
  gw_monthly <- monthly_mean(gw_level_dv)
  expect_true(all(names(gw_monthly) %in% c("year", "month", "mean_va", "n_days",  
                                           "mid_date")))
  
  expect_type(gw_monthly$year, "double")
  expect_type(gw_monthly$month, "double")
  expect_type(gw_monthly$mean_va, "double")
  expect_type(gw_monthly$n_days, "integer")
  expect_true(gw_monthly$mid_date[1] == as.Date("1978-10-15"))
  
  seg_df_month <- HASP:::create_segs(trend_results,
                                     gw_monthly, 
                                     date_col = "mid_date",
                                     value_col = "mean_va")
  
  expect_true(all(seg_df_month$x1 == c("2015-03-17", "1978-10-26")))
  expect_true(all(seg_df_month$x2 == c("2025-03-15", "2025-03-15")))
  
  expect_true(all(seg_df_month$years == c("10-year trend", 
                                          "Period of record")))
  expect_true(all(seg_df_month$trend == c("Up", "Down")))
  expect_true(all(signif(seg_df_month$y1, digits = 3) == c(-24.1, -18.1)))
  expect_true(all(signif(seg_df_month$y2, digits = 3) == c(-11.6, -24.6)))
  
  
})

test_that("utils", {
  
  no_zero <- HASP:::zero_on_top(c(1:10))
  yes_zero <- HASP:::zero_on_top(c(-10:-2))
  crosses_zero <- HASP:::zero_on_top(c(-10:5))
  
  expect_false(no_zero)
  expect_true(yes_zero)
  expect_true(is.na(crosses_zero))
  
  ld <- last_day(c("2020-12-28", "2020-02-15", "2019-02-15"))
  expect_equal(as.Date(c("2020-12-31", "2020-02-29", "2019-02-28")),
               ld)
  
  md <- mid_month(c("2019-02-15", "2020-03-08", "2010-06-01"))
  expect_equal(as.Date(c("2019-02-15", "2020-03-16", "2010-06-15")),
               md)
  
  fd <- first_day(c("2020-12-28", "2020-02-15", "2019-02-15"))
  expect_equal(as.Date(c("2020-12-01", "2020-02-01", "2019-02-01")),
               fd)
})
