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
  
  expect_equal(nrow(local_aqfr), 8751)
  expect_equal(ncol(local_aqfr), 2)
  expect_true(all(names(local_aqfr) %in%
                    c("aqfr_cd",
                      "Aqfr_Name_prpr")))
  
  expect_equal(nrow(L2701_example_data[["Daily"]]), 16275)
  expect_equal(nrow(L2701_example_data[["Discrete"]]), 1461)
  expect_equal(nrow(L2701_example_data[["QW"]]), 475)
  
  expect_equal(ncol(L2701_example_data[["Daily"]]), 11)
  expect_equal(ncol(L2701_example_data[["Discrete"]]), 14)
  expect_equal(ncol(L2701_example_data[["QW"]]), 65)
  
})

 # test_that("Get Data", {
 #   skip_on_cran()
 #   skip_on_ci()
 # 
 #  # end_date <- "2019-12-31"
 #  # state_date <- "2018-12-31"
 #  # 
 #  # aquiferCd <- "S100CSLLWD"
 #  # Let's add this later...it's really sloooow:
 #  # aq_data <- get_aquifer_data(aquiferCd, state_date, end_date)
 #  # 
 #  # expect_type(aq_data, "list")
 #  # expect_true(all(names(aq_data) %in% c("value",
 #  #                                       "lev_dt",
 #  #                                       "site_no",
 #  #                                       "state_call",
 #  #                                       "parameter_cd",
 #  #                                       "water_year",
 #  #                                       "year" )))
 #  # 
 #  # st_data <- get_state_data("AL", aquiferCd,
 #  #                           startDate = state_date, 
 #  #                           endDate = end_date)
 #  # 
 #  # expect_type(st_data, "list")
 #  # expect_true(all(names(st_data) %in% c("value",
 #  #                                       "lev_dt",
 #  #                                       "site_no",
 #  #                                       "state_call",
 #  #                                       "parameter_cd",
 #  #                                       "water_year",
 #  #                                       "year" )))
 # 
 # 
 # 
 # })

test_that("Get site summaries", {
  skip_on_cran()
  
  siteID <- "263819081585801"
  
  site_metadata <- site_summary(siteID)
  expect_true(all(c("site_no", "station_nm", "lat_va",
                    "long_va", "state_cd", "county_cd",
                    "huc_cd", "nat_aqfr_cd", "aqfr_cd",      
                    "land_net_ds", "well_depth_va", "alt_va",       
                    "alt_datum_cd", "state", "county",       
                    "nat_aq", "local_aq", "lat_deg",      
                    "lat_min", "lat_sec", "long_deg",     
                    "long_min", "long_sec") %in% names(site_metadata)))
  
  expect_equal(nrow(site_metadata), 1)
  
  site_data_available <- data_available(siteID)
  
  expect_true(all(c("Data Type", "Begin Date", "End Date", "Count") %in%
                    names(site_data_available)))
  
  expect_gte(nrow(site_data_available), 6)
  
})

test_that("Data setup", {

  
  s1 <- HASP:::set_up_data(gw_level_dv = L2701_example_data[["Daily"]],
                    gwl_data = L2701_example_data[["Discrete"]], 
                    parameter_cd = "62610", 
                    date_col = c("time", "time"),
                    value_col = c("value", "value"),
                    approved_col = c("approval_status", "approval_status"))
  
  expect_true(all(names(s1) %in% c("gw_level_dv", "gwl_data", "includes")))
  expect_true(all(s1$includes))
  expect_true(is.numeric(s1$gw_level_dv$Value))
  expect_true(is.character(s1$gw_level_dv$Approve))
  expect_true(inherits(s1$gw_level_dv$Date, "Date"))
  expect_true(is.numeric(s1$gwl_data$Value))
  expect_true(is.character(s1$gwl_data$Approve))
  expect_true(inherits(s1$gwl_data$Date, "Date"))

  incomplete_date <-  L2701_example_data[["Daily"]]
  incomplete_date$time <- as.character(incomplete_date$time)
  incomplete_date$time[1] <- "1980"
  expect_warning(s2 <- HASP:::set_up_data(gw_level_dv = incomplete_date,
                    gwl_data = L2701_example_data[["Discrete"]], 
                    parameter_cd = "62610", 
                    date_col = c("time", "time"),
                    value_col = c("value", "value"),
                    approved_col = c("approval_status", "approval_status")))
  
  incomplete_date <-  L2701_example_data[["Discrete"]]
  incomplete_date$time <- as.character(incomplete_date$time)
  incomplete_date$time[1] <- "1980"
  expect_warning(s2 <- HASP:::set_up_data(gw_level_dv = L2701_example_data[["Daily"]],
                                          gwl_data = incomplete_date, 
                                          parameter_cd = "62610", 
                                          date_col = c("time", "time"),
                                          value_col = c("value", "value"),
                                          approved_col = c("approval_status", "approval_status")))
  
})
