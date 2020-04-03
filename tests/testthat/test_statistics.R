context("Compute Statistical Tests")

test_that("Kendall Seasonal Trend", {
  
  gw_level_data <- L2701_example_data$Discrete
    
  less_data <- dplyr::filter(gw_level_data,
                             lev_dt > as.Date("2010-01-01"),
                             lev_dt < as.Date("2014-01-01"))
  
  test1 <- kendell_test_5_20_years(gw_level_data)
  test2 <- expect_message(kendell_test_5_20_years(less_data))
  
  expect_true(all(c("test", "tau", "pValue", "slope", "intercept", "trend") %in% names(test1)))
  expect_true(all(c("test", "tau", "pValue", "slope", "intercept", "trend") %in% names(test2)))
  
  expect_true(all(is.na(c(test2$tau, test2$pValue, test2$slope, test2$intercept, test2$trend))))
  expect_true(-0.0411 == signif(test1$tau[2], digits = 3))
  expect_true(0.489 == signif(test1$pValue[2], digits = 3))
  expect_true(-0.0560 == signif(test1$slope[2], digits = 3))
  expect_true(102 == signif(test1$intercept[2], digits = 3))
  expect_true("Not significant" == test1$trend[2])
  
  test3 <- kendell_test_5_20_years(gw_level_data, seasonal = FALSE)
  expect_true(0.0625 == signif(test3$tau[2], digits = 3))
  
  qw_data <- L2701_example_data$QW
  test4 <- kendell_test_5_20_years(qw_data, seasonal = FALSE, 
                                       enough_5 = 1, enough_20 = 1,
                                       date_col = "sample_dt", value_col = "result_va")
  expect_true(0.0658 == signif(test4$slope[1], digits = 3))
  expect_true(0.014 == signif(test4$slope[2], digits = 3))
  
})

test_that("Weekly frequency table", {
  
  wft <- weekly_frequency_table(L2701_example_data$Daily, 
                                p_code_dv = "62610",
                                statCd = "00001")
  expect_equal(head(wft$p25, 6), c(-28.7, -29.1, -29.3, -29.7, -29.8, -29.1), tolerance = 0.05)
  expect_equal(head(wft$nYears, 6), c(40, 41, 41, 41, 42, 42))
  expect_equal(tail(wft$minMed, 6), c(-42.2, -42.4, -42.8, -42.7, -42.5, -40.9), tolerance = 0.05)
  
})

test_that("Monthly frequency table", {
  
  mft <- monthly_frequency_table(L2701_example_data$Discrete)
  expect_equal(head(mft$p75, 6), c(-20.3, -22.6, -24.1, -26.2, -29.4, -24.3), tolerance = 0.05)
  expect_equal(head(mft$nYears, 6), c(22, 29, 26, 33, 28, 30))
  expect_equal(tail(mft$maxMed, 6), c(-16.7, -12.3, -15.3, -4.83, -10.8, -16.2), tolerance = 0.05)
  
})

test_that("Daily summary table", {
  
  daily_summary_table <- daily_gwl_summary(L2701_example_data$Daily, "62610", "00001")
  expect_equal(nrow(daily_summary_table), 1)
  expect_equal(daily_summary_table$percent_complete, 95)
  expect_equal(daily_summary_table$begin_date, as.Date("1978-10-01"))
  expect_equal(daily_summary_table$p25, -28.56, tolerance = 0.005)
  expect_equal(daily_summary_table$highest_level, -2.81, tolerance = 0.05)
  
})

test_that("Daily frequency table", {
  
  daily_frequency_table <- daily_frequency_table(L2701_example_data$Daily, "62610", "00001")
  expect_equal(daily_frequency_table$max[1], -5.29, tolerance = 0.005)
  expect_equal(daily_frequency_table$min[365], -42.2, tolerance = 0.05)
  expect_equal(daily_frequency_table$mean[183], -24.3, tolerance = 0.05)
  
})
