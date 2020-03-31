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

  site_data <- L2701_example_data$QW
  
  sc_plot <- Sc_Cl_plot(site_data, title = "Hi")
  expect_true(all(c("Date", "chloride", "sp") %in%
                    names(sc_plot$data)))
  
  sccl_table <- Sc_Cl_table(site_data)
  expect_true(all(c("Date",   
                    "chloride",
                    "sp" ) %in% names(sccl_table)))
  
})

test_that("Monthly frequency plot", {
  
  skip_on_cran()
  
  plot <- monthly_frequency_plot(L2701_example_data$Discrete)
  
  plot_data_elements <- purrr::map(plot$layers, "data") %>%
    purrr::map(names) %>%
    unlist()
  
  expect_true(all(c("plot_month", "x", "y", "ymin", "group") %in%
                plot_data_elements))
  
})

test_that("Weekly frequency plot", {
  
  skip_on_cran()
  
  site <- "263819081585801"
  parameterCd <- "62610"
  statCd <- "00001"
  plot <- weekly_frequency_plot(L2701_example_data$Daily, parameterCd, statCd, title = "Groundwater Level")
  
  plot_data_elements <- purrr::map(plot$layers, "data") %>%
    purrr::map(names) %>%
    unlist()
  
  expect_true(all(c("plot_week", "x", "y", "ymin", "group", "plot_week_last") %in%
                    plot_data_elements))
  
})

test_that("Periodic gwl plot", {
  
  gwl_data <- L2701_example_data$Discrete
  title <- attr(gwl_data, "siteInfo")[["station_nm"]]
  plot_out <- gwl_plot_periodic(gwl_data, title)
  
  dv <- L2701_example_data$Daily
  plot2 <- gwl_plot_all(dv, gwl_data, "title")
  
  expect_true(all(c("lev_dt", "sl_lev_va", "lev_age_cd") %in%
                    names(plot_out[["data"]])))
  
  expect_true(all(c("data", "layers", "scales",     
                    "mapping", "theme", "coordinates",
                    "facet","plot_env", "labels"   ) %in%
                    names(plot2)))
  
})


  