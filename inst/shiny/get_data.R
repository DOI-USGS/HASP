rawData_data <- reactiveValues(data = NULL,
                               example_data = FALSE, 
                               aquifer_cd = NULL)

observeEvent(input$example_data,{
  rawData_data$example_data <- TRUE
  
  aquifer_data <- HASP::aquifer_data

  updateNumericInput(session = session, 
                     inputId = "end_year", value = max(aquifer_data[["year"]], na.rm = TRUE),
                     max = max(max(aquifer_data[["year"]], na.rm = TRUE), na.rm = TRUE))
  updateNumericInput(session = session, 
                     inputId = "start_year", value = min(aquifer_data$year, na.rm = TRUE),
                     min = min(aquifer_data$year, na.rm = TRUE))
  
  rawData_data$data <- aquifer_data
  rawData_data$aquifer_cd <- "Basin and Range basin-fill aquifers"
  updateSelectInput(session = session, inputId = "aquiferCd", 
                    selected = "Basin and Range basin-fill aquifers")

})

observeEvent(input$get_data,{
  rawData_data$example_data <- FALSE
  orig_choice <- rawData_data$aquifer_cd
  
  showNotification("Loading", duration = NULL, id = "load")

  aquiferCd <- input$aquiferCd
  
  short_code <- summary_aquifers$nat_aqfr_cd[summary_aquifers$long_name == aquiferCd]
  end_date <- Sys.Date()
  parts <- strsplit(as.character(end_date), split = "-")[[1]]
  parts[[1]] <- as.character(as.numeric(parts[[1]]) - 31) #gives a buffer
  start_date <- paste(parts, collapse = "-")
  
  states <- unlist(summary_aquifers$states[summary_aquifers$long_name == aquiferCd])
  
  aquifer_data <- data.frame()
  site_data <- data.frame()

  for(state in states){
    
    id_message <- showNotification(paste("Getting data from: ", state), 
                     type = "message", duration = NULL)
    
    state_data <- tryCatch(
      expr = {
        HASP:::get_state_data(state = state, 
                                    aquiferCd = short_code, 
                                    startDate = start_date,
                                    endDate = end_date)
      }, 
      error = function(e){ 
        cat(state, "errored \n")
        showNotification(paste("No data from: ", state), 
                         type = "message", duration = 5, id = state)
      }
    )
    removeNotification(id = id_message)
    
    if(!all(is.na(state_data$site_no))){
      state_data_sites <- dataRetrieval::readNWISsite(unique(state_data$site_no))
      
      state_data_sites <- state_data_sites %>% 
        select(station_nm, site_no, dec_lat_va, dec_long_va, coord_datum_cd)
      
      aquifer_data <- bind_rows(aquifer_data, state_data)
      site_data <- bind_rows(site_data, state_data_sites)
    }
    
  }

  updateNumericInput(session = session, 
                     inputId = "end_year", value = max(aquifer_data$year, na.rm = TRUE),
                     max = max(aquifer_data$year, na.rm = TRUE))
  updateNumericInput(session = session, 
                     inputId = "start_year", value = min(aquifer_data$year, na.rm = TRUE),
                     min = min(aquifer_data$year, na.rm = TRUE))
  
  if(nrow(aquifer_data) == 0){
    showNotification(paste("Not enough data for: ", aquiferCd), 
                     type = "message", duration = 5)
    updateSelectInput(session = session, selected = orig_choice,
                      inputId = "aquiferCd")
    
  } else {
    attr(aquifer_data, "siteInfo") <- site_data
    rawData_data$aquifer_cd <- aquiferCd
    rawData_data$data <- aquifer_data
  }

  removeNotification(id = state)
  removeNotification(id = "load")
  
})


rawData <- reactive({
  
  return(rawData_data$data)
  
})