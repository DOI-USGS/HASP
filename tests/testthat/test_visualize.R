context("Visualize Data")

test_that("Graphs", {
  aquifer_data <- aquifer_data
  sum_col <- "lev_va"
  num_years <- 30
 
  comp_data <- plot_composite_data(aquifer_data, sum_col, num_years)
  norm_data <- plot_normalized_data(aquifer_data, sum_col, num_years)
  
  expect_true(all(names(comp_data$data) %in% c("year","name","value")))
  expect_true(all(names(norm_data$data) %in% c("year","name","value")))

  expect_true(all(levels(comp_data$data$name) %in% c("Composite Annual Median", "Composite Annual Mean")))
  expect_true(all(levels(norm_data$data$name) %in% c("Composite Annual Median Percent Variation",
                                                     "Composite Annual Mean Percent Variation")))
  
    
})
  
test_that("Map", {
  aquifer_data <- aquifer_data
  sum_col <- "lev_va"
  num_years <- 30
  
  map <- map_hydro_data(aquifer_data, sum_col, num_years)
  
  expect_true(all(class(map) %in% c("leaflet","htmlwidget")))

  
})
  