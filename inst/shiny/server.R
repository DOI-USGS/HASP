library(shiny)
library(HASP)
options(shiny.maxRequestSize = 200 * 1024^2)
library(dplyr)
library(tidyr)
library(leaflet)

source("modules.R",local=TRUE)

shinyServer(function(input, output, session) {

  
  observe({
    if (input$close > 0) shiny::stopApp()    
  })

  source("get_data.R",local=TRUE)$value
  source("comp_plot.R",local=TRUE)$value
  source("norm_plot.R",local=TRUE)$value

  output$mymap <- leaflet::renderLeaflet({

    map <- leaflet::leaflet() %>%
      leaflet::addProviderTiles("CartoDB.Positron") %>%
      leaflet::setView(lng = -83.5, lat = 44.5, zoom=6)   


  })
  
  observe({
    validate(
      need(!is.null(rawData_data$data), "Please select a data set")
    )
    req(input$mainOut == "map")
    
    showNotification("Prepping map", id = "loadmap",
                     type = "message")
    
    aquifer_data <- rawData()
    
    shinyAce::updateAceEditor(session, editorId = "map_code", value = map_code() )
    
    x <- filter_sites(aquifer_data, num_years = 30)
    
    if(nrow(x) == 0){
      x <- aquifer_data
    }
    
    map_data <- prep_map_data(x)

    map <- leafletProxy("mymap", data = map_data) %>%
      clearMarkers() %>%
      addCircleMarkers(lat=~dec_lat_va, lng=~dec_long_va,
                       radius = 3,
                       fillOpacity = 1,
                       popup= ~popup,
                       stroke=FALSE) %>% 
      fitBounds(~min(dec_long_va), ~min(dec_lat_va),
                ~max(dec_long_va), ~max(dec_lat_va))
    
    removeNotification(id = "loadmap")
    
    map
    
  })

  # observe({
  #   gwl_data <- rawData_data$data
  # 
  #   if(all(is.na(gwl_data$sl_lev_va))){
  #     updateRadioButtons(session, inputId = "gwl_vals", selected = "lev_va")
  #   }
  #   
  #   if(all(is.na(gwl_data$lev_va))){
  #     updateRadioButtons(session, inputId = "gwl_vals", selected = "sl_lev_va")
  #   }
  # 
  # })
  
  callModule(graph_download_code, 'composite_graph', 
             plot_gg = comp_plot, 
             code_out = comp_plot_out, 
             raw_data = reactive({rawData_data$data}))
  
  callModule(graph_download_code, 'normalized_graph', 
             plot_gg = norm_plot, 
             code_out = norm_plot_out, 
             raw_data = reactive({rawData_data$data}))
 
  setup <- reactive({
    end_date <- Sys.Date()
    parts <- strsplit(as.character(end_date), split = "-")[[1]]
    parts[[1]] <- as.character(as.numeric(parts[[1]]) - 30)
    start_date <- paste(parts, collapse = "-")
    long_name <- input$aquiferCd
    aquiferCd <- summary_aquifers$nat_aqfr_cd[summary_aquifers$long_name == long_name]
    
    year_start <- input$start_year
    year_end <- input$end_year
    
    if(rawData_data$example_data){
      setup_code <- paste0('library(HASP)
aquifer_data <- aquifer_data')
    } else {
      setup_code <- paste0('library(HASP)
long_name <- "', input$aquiferCd ,'"
aquiferCd <- summary_aquifers$nat_aqfr_cd[summary_aquifers$long_name == long_name]
aquifer_data <- get_aquifer_data(aquiferCd = "',aquiferCd,'",
                           startDate = "', start_date,'",
                           endDate = "', end_date, '")
aquifer_data <- filter_sites(aquifer_data, 
                             start_year = ',year_start,',
                             end_year = ', year_end,')')      
    }
    
    setup_code
  })
  
  map_code <- reactive({
    paste0(setup(),'
map_data <- map_hydro_data(aquifer_data, 
                           sum_col = "lev_va", num_years = 30)
map_data')
           
  })
  


  session$onSessionEnded(stopApp)
  
})
