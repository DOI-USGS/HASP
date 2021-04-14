context("Compute Statistical Tests")

test_that("Kendall Seasonal Trend", {
  
  gw_level_data <- L2701_example_data$Discrete
    
  less_data <- dplyr::filter(gw_level_data,
                             lev_dt > as.Date("2010-01-01"),
                             lev_dt < as.Date("2014-01-01"))
  
  test1 <- kendell_test_5_20_years(gw_level_data, parameter_cd_gwl = "62610")
  test2 <- expect_message(kendell_test_5_20_years(less_data, parameter_cd_gwl = "62610"))
  
  expect_true(all(c("test", "tau", "pValue", "slope", "intercept", "trend") %in% names(test1)))
  expect_true(all(c("test", "tau", "pValue", "slope", "intercept", "trend") %in% names(test2)))
  
  expect_true(all(is.na(c(test2$tau, test2$pValue, test2$slope, test2$intercept, test2$trend))))
  expect_true(0.0954 == signif(test1$tau[2], digits = 3))
  expect_true(0.0254 == signif(test1$pValue[2], digits = 3))
  expect_true(0.22 == signif(test1$slope[2], digits = 3))
  expect_true(-435 == signif(test1$intercept[2], digits = 3))
  expect_true("Up" == test1$trend[2])
  
  test3 <- kendell_test_5_20_years(gw_level_data, seasonal = FALSE,
                                   parameter_cd_gwl = "62610")
  expect_true(0.152 == signif(test3$tau[2], digits = 3))
  
  qw_data <- L2701_example_data$QW
  test4 <- kendell_test_5_20_years(qw_data, seasonal = FALSE, 
                                       enough_5 = 1, enough_20 = 1,
                                       date_col = "sample_dt", value_col = "result_va")
  expect_true(0.0658 == signif(test4$slope[1], digits = 3))
  expect_true(0.014 == signif(test4$slope[2], digits = 3))
  
})

test_that("Weekly frequency table", {
  
  wft <- weekly_frequency_table(L2701_example_data$Daily, 
                                date_col = "Date",
                                value_col = "X_62610_00001",
                                approved_col = "X_62610_00001_cd")
  expect_equal(as.numeric(head(wft$p25, 6)), c(-28.7, -29.1, -29.3, -29.7, -29.8, -29.1), tolerance = 0.05)
  expect_equal(as.numeric(head(wft$nYears, 6)), c(40, 41, 41, 41, 42, 42))
  expect_equal(tail(wft$minMed, 6), c(-42.2, -42.4, -42.8, -42.7, -42.5, -40.9), tolerance = 0.05)
  
})

test_that("Monthly frequency table", {
  
  mft <- monthly_frequency_table(L2701_example_data$Daily,
                                 date_col = "Date",
                                 value_col = "X_62610_00001",
                                 approved_col = "X_62610_00001_cd")
  expect_equal(as.numeric(head(mft$p75, 6)),
               c(-18.0575, -19.0500, -20.2625, -22.0500, -23.5100, -21.0200),
               tolerance = 0.05)
  expect_equal(as.numeric(head(mft$nYears, 6)),
               c(41, 42, 42, 42, 42, 41))
  expect_equal(tail(mft$maxMed, 6),
               c(-6.790, -4.820, -3.240, -4.800, -4.535, -4.860),
               tolerance = 0.05)
  
})

test_that("Daily summary table", {
  
  daily_summary_table <- daily_gwl_summary(L2701_example_data$Daily,
                                           date_col = "Date",
                                           value_col = "X_62610_00001",
                                           approved_col = "X_62610_00001_cd")
  expect_equal(nrow(daily_summary_table), 1)
  expect_equal(daily_summary_table$percent_complete, 95)
  expect_equal(daily_summary_table$begin_date, as.Date("1978-10-01"))
  expect_equal(daily_summary_table$p25, -28.56, tolerance = 0.005)
  expect_equal(daily_summary_table$highest_level, -2.81, tolerance = 0.05)
  
})

test_that("Daily frequency table", {
  
  daily_frequency_table <- daily_frequency_table(L2701_example_data$Daily,
                                                 date_col = "Date",
                                                 value_col = "X_62610_00001",
                                                 approved_col = "X_62610_00001_cd")
  expect_equal(daily_frequency_table$max[1], -5.29, tolerance = 0.005)
  expect_equal(daily_frequency_table$min[365], -42.2, tolerance = 0.05)
  expect_equal(daily_frequency_table$mean[183], -24.3, tolerance = 0.05)
  
})

test_that("trend segments",{
  
  gw_level_dv <- L2701_example_data$Daily
  qw_data <- L2701_example_data$QW
  
  seg_df <- create_segs(qw_data)
  expect_true(all(seg_df$x1 == c(2014,1999)))
  expect_true(all(seg_df$x2 == c(2019,2019)))
  expect_true(all(seg_df$years == c(5,20)))
  expect_true(all(seg_df$trend == c("5-year trend",
                                 "20-year trend")))
  
  seg_df_dv <- create_segs(gw_level_dv, 
                        date_col = "Date", 
                        value_col = "X_62610_00001")
  expect_true(all(seg_df_dv$x1 == c(2015,2000)))
  expect_true(all(seg_df_dv$x2 == c(2020,2020)))
  expect_true(all(seg_df_dv$years == c(5,20)))
  expect_true(all(seg_df_dv$trend == c("5-year trend",
                                    "20-year trend")))
  expect_true(all(signif(seg_df_dv$y1, digits = 3) == c(-25.4, -29.5)))
  expect_true(all(signif(seg_df_dv$y2, digits = 3) == c(-17, -23.9)))
  
  gw_monthly <- monthly_mean(gw_level_dv, 
                             date_col = "Date", 
                             value_col = "X_62610_00001")
  expect_true(all(names(gw_monthly) %in% c("year", "month", "mean_va", "n_days",  
                                           "mid_date")))
  
  expect_type(gw_monthly$year, "double")
  expect_type(gw_monthly$month, "double")
  expect_type(gw_monthly$mean_va, "double")
  expect_type(gw_monthly$n_days, "integer")
  expect_true(gw_monthly$mid_date[1] == as.Date("1978-10-15"))
  
  seg_df_month <- create_segs(gw_monthly, 
                        date_col = "mid_date",
                        value_col = "mean_va")
  
  expect_true(all(seg_df_month$x1 == c(2015,2000)))
  expect_true(all(seg_df_month$x2 == c(2020,2020)))
  expect_true(all(seg_df_month$years == c(5,20)))
  expect_true(all(seg_df_month$trend == c("5-year trend",
                                       "20-year trend")))
  expect_true(all(signif(seg_df_month$y1, digits = 3) == c(-26, -29.5)))
  expect_true(all(signif(seg_df_month$y2, digits = 3) == c(-17.5, -24.1)))
  
  
})

test_that("utils", {
  
  no_zero <- zero_on_top(c(1:10))
  yes_zero <- zero_on_top(c(-10:-2))
  crosses_zero <- zero_on_top(c(-10:5))
  
  expect_false(no_zero)
  expect_true(yes_zero)
  expect_true(is.na(crosses_zero))
  
})
