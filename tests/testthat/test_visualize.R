context("Visualize Data")

test_that("Composite Graphs", {
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


test_that("SC Chloride graphs and table", {
  
  skip_on_cran()
  
  site <- '263819081585801'
  site_data <- dataRetrieval::readNWISqw(site, 
                                         parameterCd = c("00095","90095","00940","99220"))
  expect_silent(Sc_Cl_plot(site_data, title = "Hi"))
  
  sccl_table <- Sc_Cl_table(site_data)
  expect_true(all(c("Date",   
                    "chloride",
                    "sp" ) %in% names(sccl_table)))
  
})


  