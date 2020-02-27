context("Sample Data")

test_that("Data", {
  expect_equal(nrow(summary_aquifers), 68)
  expect_equal(ncol(summary_aquifers), 4)
  expect_true(all(names(summary_aquifers) %in%
                  c("long_name",
                    "nat_aqfr_cd", 
                    "state_indexes",
                    "states")))
  
  expect_equal(nrow(nat_aqfr_state), 368)
  expect_equal(ncol(nat_aqfr_state), 5)
  expect_true(all(names(nat_aqfr_state) %in%
                    c("long_name",
                      "nat_aqfr_cd", 
                      "Country",
                      "State",
                      "other_cd")))
  
  
  expect_equal(nrow(HASP:::markerTable), 5)
  expect_equal(ncol(HASP:::markerTable), 2)
  expect_true(all(names(HASP:::markerTable) %in%
                    c("Trend",
                      "MarkerDescription")))
  expect_equal(nrow(HASP:::markerTable2), 15)
  expect_equal(ncol(HASP:::markerTable2), 4)
  expect_true(all(names(HASP:::markerTable2) %in%
                    c("trendType",
                      "trend",
                      "markerDescription",
                      "r_lwd")))
  
})

test_that("Get Data", {
  skip_on_cran()
  
  end_date <- "2019-12-31"
  state_date <- "1989-12-31"

  aquiferCd <- "S100CSLLWD"
  aq_data <- get_aquifer_data(aquiferCd, state_date, end_date)  
  
  expect_type(aq_data, "list")
  expect_true(all(names(aq_data) %in% c("lev_va",
                                        "sl_lev_va",
                                        "lev_dt",
                                        "site_no",
                                        "state_call",
                                        "year" )))
})


