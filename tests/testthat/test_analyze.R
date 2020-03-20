context("Analyze Data")

test_that("Year summaries", {
  aquifer_data <- aquifer_data

  summary_info2 <- all_year_summaries(aquifer_data, "lev_va")
  expect_type(summary_info2, "list")
  
  expect_true(all(names(summary_info2) %in% 
                    c("site_no", "min_site",
                      "max_site",  "mean_site",
                      "p10", "p25", "p75",      
                      "p90",  "count")))
  
  expect_equal(round(summary_info2$min_site[1], digits = 2), 3.26)
  expect_equal(round(summary_info2$max_site[1], digits = 2), 7.9)
  expect_equal(round(summary_info2$mean_site[1], digits = 2),5.37)
  expect_equal(round(summary_info2$p10[1], digits = 2), 4.43)
  expect_equal(round(summary_info2$p25[1], digits = 2), 4.57)
  expect_equal(round(summary_info2$p75[1], digits = 2), 6.46)
  expect_equal(round(summary_info2$p90[1], digits = 2), 7.12)
  
  
})


test_that("Map info", {
  aquifer_data <- aquifer_data
  sum_col <- "lev_va"
  map_info <- prep_map_data(aquifer_data, sum_col)
  
  expect_true(all(c("popup","station_nm", 
                "dec_long_va", "dec_lat_va") %in% 
                names(map_info)))
  
  expect_equal(map_info$popup[1],
               "<b><a href=\"https://waterdata.usgs.gov/monitoring-location/312127110073101\">312127110073101</a></b><br/>\n             <table>\n             <tr><td>Name:</td><td>D-24-22 08DBA1 [PLS-LI]</td></tr>\n             </table>")
  
})


test_that("Filter sites", {
  
  aquifer_data <- aquifer_data
  sum_col <- "lev_va"
  num_years <- 30
  
  aq_data <- filter_sites(aquifer_data, sum_col, num_years)
  
  expect_true(nrow(aquifer_data) > nrow(aq_data))
  
  freq <- aq_data %>%
    group_by(site_no) %>% 
    summarise(nYear = length(unique(year))) 
  
  expect_true(all(freq$nYear >= 30))
  
})


test_that("Composite hydrodata", {
  
  aquifer_data <- aquifer_data
  sum_col <- "lev_va"
  num_years <- 30
  
  comp_data <- composite_data(aquifer_data, sum_col, num_years)
  
  expect_true(all(names(comp_data) %in% c("year", "name", "value")))
  expect_true(all(levels(comp_data$name) %in% c("Composite Annual Median",
                                                "Composite Annual Mean")))
  expect_equal(round(comp_data$value[1], digits = 2), 155.82)
})

test_that("Normalized composite hydrodata", {
  
  aquifer_data <- aquifer_data
  sum_col <- "lev_va"
  num_years <- 30
  
  norm_data <- normalized_data(aquifer_data, sum_col, num_years)
  
  expect_true(all(names(norm_data) %in% c("year", "name", "value")))
  expect_true(all(levels(norm_data$name) %in% c("Composite Annual Median Percent Variation",
                                                "Composite Annual Mean Percent Variation")))
  expect_equal(round(norm_data$value[1], digits = 2), 0.04)
})
