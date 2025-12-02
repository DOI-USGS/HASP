context("Analyze Data")

format_2 <- function(x){
  return(as.numeric(round(x, digits = 2)))
}

test_that("Site summaries", {
  aquifer_data <- aquifer_data
  aquifer_data <- aquifer_data[aquifer_data$parameter_cd == "72019", ]
  summary_info2 <- site_data_summary(aquifer_data, site_col = "site_no")

  expect_type(summary_info2, "list")
  
  expect_true(all(names(summary_info2) %in% 
                    c("site", "min_site",
                      "max_site",  "mean_site",
                      "p10", "p25", "p75", "p50",     
                      "p90",  "count")))
  
  expect_equal(format_2(summary_info2$min_site[1]), 3.26)
  expect_equal(format_2(summary_info2$max_site[1]), 7.93)
  expect_equal(format_2(summary_info2$mean_site[1]),5.4)
  expect_equal(format_2(summary_info2$p10[1]), 4.45)
  expect_equal(format_2(summary_info2$p25[1]), 4.57)
  expect_equal(format_2(summary_info2$p75[1]), 6.51)
  expect_equal(format_2(summary_info2$p90[1]), 7.13)
  
  
})

test_that("QW summaries", {
  
  qw_data <- L2701_example_data$QW
  
  x <- qw_summary(qw_data, 
                  CharacteristicName = "Chloride", 
                  norm_range = c(225,999))
  
  expect_true(all(x$Analysis == c("Date of first sample",                           
                                     "First sample result (mg/l)",                     
                                     "Date of last sample",                            
                                     "Last sample result (mg/l)",                      
                                     "Date of first sample within  225 to 999 mg/l",   
                                     "Date of first sample with  1000 mg/l or greater",
                                     "Minimum (mg/l)",                                 
                                     "Maximum (mg/l)",                                 
                                     "Mean (mg/l)",                                    
                                     "First quartile (mg/l)",                          
                                     "Median (mg/l)",                                  
                                     "Third quartile (mg/l)",                          
                                     "Number of samples")))
  
  expect_true(all(x$Result == c("1978-09-06", "52", "2022-05-18", "72", "",          
                                  "", "14", "119", "60.9", "54",        
                                  "58", "68", "81")))
  
  y <- qw_summary(qw_data, 
                  CharacteristicName = "Specific conductance",
                  norm_range = NA)
  
  expect_true(all(y$Analysis == c("Date of first sample", "First sample result (uS/cm @25C)",
                                   "Date of last sample", "Last sample result (uS/cm @25C)", 
                                   "Minimum (uS/cm @25C)", "Maximum (uS/cm @25C)",            
                                   "Mean (uS/cm @25C)", "First quartile (uS/cm @25C)",     
                                   "Median (uS/cm @25C)", "Third quartile (uS/cm @25C)",     
                                   "Number of samples")))
  
  expect_true(all(y$Result == c("1979-05-09", "580", "2022-05-18", "606", "176",       
                                "785", "553", "544", "560", "569",       
                                "394")))
  
  
})

test_that("Map info", {
  site_info <- site_info
  map_info <- prep_map_data(site_info)
  
  expect_true(all(c("popup","monitoring_location_id", 
                "geometry") %in% 
                names(map_info)))
  
  expect_equal(map_info$popup[1],
               "<b><a href=\"https://waterdata.usgs.gov/monitoring-location/AZ014-314322110030901\">AZ014-314322110030901</a></b><br/>\n             <table>\n             <tr><td>Name:</td><td>D-20-22 01CAA</td></tr>\n             </table>")
  
})


test_that("Filter sites", {
  
  aquifer_data <- aquifer_data
  num_years <- 30
  
  aq_data <- filter_sites(aquifer_data, num_years, parameter_cd = "72019")
  
  expect_true(nrow(aquifer_data) > nrow(aq_data))
  
  freq <- aq_data |> 
    dplyr::group_by(monitoring_location_id) |> 
    dplyr::summarise(nYear = length(unique(year))) 
  
  expect_true(all(freq$nYear >= 30))
  
})


test_that("Composite hydrodata", {
  
  aquifer_data <- aquifer_data
  num_years <- 30
  
  comp_data <- composite_data(aquifer_data, num_years, parameter_cd = "72019")
  
  expect_true(all(names(comp_data) %in% c("year", "name", "value")))
  expect_true(all(levels(comp_data$name) %in% c("Median",
                                                "Mean")))
  expect_equal(format_2(comp_data$value[1]), 150.89)
})

test_that("Normalized composite hydrodata", {
  
  aquifer_data <- aquifer_data
  num_years <- 30
  
  norm_data <- normalized_data(aquifer_data, num_years, parameter_cd = "72019")
  
  expect_true(all(names(norm_data) %in% c("year", "name", "value")))
  expect_true(all(levels(norm_data$name) %in% c("Median",
                                                "Mean")))
  expect_equal(format_2(norm_data$value[1]), 0.1)
})


test_that("Create html reports", {

  report_folder <- system.file("extdata", package="HASP")
  
  report_name <-  "My_sample_report_test"
  
  path_to_rmd <- file.path(report_folder, 
                           paste0(report_name, ".Rmd"))
  
  exists_before <- file.exists(path_to_rmd)
  expect_false(exists_before)
  
  create_groundwater_report(siteID = "424520070562401",
                            report_name = report_name,
                            report_folder = report_folder,
                            output_type = "html")
  
  exists_after <- file.exists(path_to_rmd)
  expect_true(exists_after)
  file.remove(path_to_rmd)
})

test_that("Create word reports", {
  
  report_folder <- system.file("extdata", package="HASP")
  
  report_name <-  "My_sample_report_test"
  
  path_to_rmd <- file.path(report_folder, 
                           paste0(report_name, ".Rmd"))
  
  exists_before <- file.exists(path_to_rmd)
  expect_false(exists_before)
  
  create_groundwater_report(siteID = "424520070562401",
                            report_name = report_name,
                            report_folder = report_folder,
                            output_type = "word")
  
  exists_after <- file.exists(path_to_rmd)
  expect_true(exists_after)
  file.remove(path_to_rmd)
})

test_that("water year", {
  
  x <- c("2010-01-01", "1994-02", "1980", "2009-11-01")
  wy <- water_year(x)
  
  expect_equal(wy, c(2010, 1994, 1980, 2010))
  expect_message(water_year(x))
})