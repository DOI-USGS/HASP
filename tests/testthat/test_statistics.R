context("Compute Statistical Tests")

test_that("Kendall Seasonal Trend", {
  
  gw_level_data <- dataRetrieval::readNWISgwl("260041080093102", 
                                              startDate = "1999-01-01",
                                              endDate = "2019-12-31")
  less_data <- dplyr::filter(gw_level_data,
                             lev_dt > as.Date("2010-01-01"),
                             lev_dt < as.Date("2014-01-01"))
  
  test1 <- seasonal_kendall_trend_test(gw_level_data)
  test2 <- expect_message(seasonal_kendall_trend_test(less_data))
  
  expect_true(all(c("test", "tau", "pValue", "slope", "intercept", "trend") %in% names(test1)))
  expect_true(all(c("test", "tau", "pValue", "slope", "intercept", "trend") %in% names(test2)))
  
  expect_true(all(is.na(c(test2$tau, test2$pValue, test2$slope, test2$intercept, test2$trend))))
  
})