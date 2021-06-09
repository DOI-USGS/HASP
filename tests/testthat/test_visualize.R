context("Visualize Data")

test_that("Composite Graphs", {
  aquifer_data <- aquifer_data
  num_years <- 30
 
  comp_data <- plot_composite_data(aquifer_data,  num_years)
  norm_data <- plot_normalized_data(aquifer_data, num_years)
  
  expect_true(all(names(comp_data$data) %in% c("year","name","value")))
  expect_true(all(names(norm_data$data) %in% c("year","name","value")))

  expect_true(all(levels(comp_data$data$name) %in% c("Median", "Mean")))
  expect_true(all(levels(norm_data$data$name) %in% c("Median",
                                                     "Mean")))
  
    
})
  
test_that("Map", {
  aquifer_data <- aquifer_data
  num_years <- 30
  
  map <- map_hydro_data(aquifer_data, num_years)
  
  expect_true(all(class(map) %in% c("leaflet","htmlwidget")))

  
})


test_that("SC Chloride graphs and table", {

  site_data <- L2701_example_data$QW
  
  sc_plot <- Sc_Cl_plot(site_data, plot_title = "Hi")
  expect_true(all(c("Date", "chloride", "sp") %in%
                    names(sc_plot$data)))
  
  sccl_table <- Sc_Cl_table(site_data)
  expect_true(all(c("Date",   
                    "chloride",
                    "sp" ) %in% names(sccl_table)))
  
  qw_plot_out <- qw_plot(site_data, "hi!")
  expect_true(all(c(c("data", "layers", "scales",     
                      "mapping", "theme", "coordinates",
                      "facet","plot_env", "labels")) %in%
                    names(qw_plot_out)))
  
})

test_that("Monthly frequency plot", {
  
  plot <- monthly_frequency_plot(L2701_example_data$Daily,
                                 date_col = "Date",
                                 value_col = "X_62610_00001",
                                 approved_col = "X_62610_00001_cd")
  
  plot_data_elements <- unlist(lapply(plot$layers, function(x) {names(x$data)}))
  
  expect_true(all(c("plot_month", "x", "y", "ymin", "group") %in%
                plot_data_elements))
  
})

test_that("Weekly frequency plot", {
  
  plot <- weekly_frequency_plot(L2701_example_data$Daily, 
                                date_col = "Date",
                                value_col = "X_62610_00001",
                                approved_col = "X_62610_00001_cd")
  
  plot_data_elements <- unlist(lapply(plot$layers, function(x) {names(x$data)}))
  
  expect_true(all(c("plot_week", "x", "y", "ymin", "group", "plot_week_last") %in%
                    plot_data_elements))
  
})

test_that("Field gwl plot", {
  
  gwl_data <- L2701_example_data$Discrete
  plot_title <- attr(gwl_data, "siteInfo")[["station_nm"]]
  plot_out <- gwl_plot_field(gwl_data, plot_title, parameter_cd_gwl = "62610")
  
  dv <- L2701_example_data$Daily
  plot2 <- gwl_plot_all(dv, gwl_data,
                        parameter_cd = "62610",
                        plot_title = "title",)
  
  expect_true(all(c("lev_dt", "sl_lev_va", "lev_age_cd") %in%
                    names(plot_out[["data"]])))
  
  expect_true(all(c("data", "layers", "scales",     
                    "mapping", "theme", "coordinates",
                    "facet","plot_env", "labels") %in%
                    names(plot2)))
  
  plot_with_trend <- gwl_plot_all(dv, gwl_data, 
                                  plot_title = "title",
                                  parameter_cd = "62610",
                                  add_trend = TRUE)
  
  plot_data_elements <- unlist(lapply(plot_with_trend$layers, function(x) {names(x$data)}))
  
  expect_true(all(c("Date", "Value", "year", 
                    "is_complete", "x1", "x2", "y1", "y2", "trend") %in%
                    plot_data_elements))
  
  plot_with_gwl <- gwl_plot_all(NULL, gwl_data, 
                                plot_title = "title",
                                parameter_cd = "62610",
                                add_trend = FALSE)
  plot_elements <- unlist(lapply(plot_with_gwl$layers, function(x) {names(x$data)}))
  expect_true(all(c("lev_dt", "sl_lev_va", "year") %in%
                    plot_elements))
  
})

test_that("Daily gwl plot", {
  
  gwl_data <- L2701_example_data$Daily
  plot_title <- attr(gwl_data, "siteInfo")[["station_nm"]]
  
  plot1 <- daily_gwl_2yr_plot(gwl_data, 
                              date_col = "Date",
                              value_col = "X_62610_00001",
                              approved_col = "X_62610_00001_cd",
                              historical_stat = "mean",
                              month_breaks = TRUE,
                              plot_title = plot_title)
  
  plot_data_elements <- unlist(lapply(plot1$layers, function(x) {names(x$data)}))
  
  expect_true(all(c("Date", "middle", "min", "max") %in%
                    plot_data_elements))
  
  plot2 <- daily_gwl_2yr_plot(gwl_data,
                              date_col = "Date",
                              value_col = "X_62610_00001",
                              approved_col = "X_62610_00001_cd",
                              historical_stat = "median",
                              month_breaks = FALSE,
                              plot_title = plot_title)
  
  plot_data_elements <- unlist(lapply(plot2$layers, function(x) {names(x$data)}))
  
  expect_true(all(c("Date", "middle", "min", "max") %in%
                    plot_data_elements))
  
})

test_that("Chloride trend graph", {
  qw_data <- L2701_example_data$QW
  title <- "Hi"
  plot_out <- trend_plot(qw_data, plot_title = title)
  plot_data_elements <- unlist(lapply(plot_out$layers, function(x) {names(x$data)}))
  
  expect_true(all(c("sample_dt", "result_va", "condition", "x1", "x2", "y1",       
                    "y2", "trend", "y") %in% plot_data_elements))
})
  