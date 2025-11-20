rawData_data <- reactiveValues(daily_data = NULL,
                               example_data = FALSE,
                               gwl_data = NULL,
                               qw_data = NULL,
                               p_code_dv = pcode_info[pcode_info$parameter_code == "62610", ],
                               stat_cd = "00001",
                               available_data = NULL,
                               site_meta = NULL,
                               p_code_qw = NULL)

clear_data <- function(){
  rawData_data$gwl_data <- NULL
  rawData_data$qw_data <- NULL
  rawData_data$daily_data <- NULL
  rawData_data$p_code_dv <- NULL
  rawData_data$stat_cd <- NULL
  rawData_data$p_code_qw <- NULL
  rawData_data$available_data <- NULL
  rawData_data$site_meta <- NULL
}

observeEvent(input$clear_data,{
  clear_data()
})

observeEvent(input$get_data_avail,{
  
  clear_data()
  
  site_id <- input$siteID
  
  site_info <- site_summary(site_id)
  
  if(!any(grepl("GW", site_info$site_type_code))){
    showNotification("The site is not identified as a groundwater site.", 
                     type = "error")
  }
  
  data_avail <- data_available(site_id)
  rawData_data$available_data <- data_avail
  rawData_data$site_meta <-  site_info

  pcodes_dv <- data_avail[data_avail$`Data Type` == "Daily", ]

  rawData_data$p_code_dv <- pcodes_dv
  rawData_data$stat_cd <- unique(data_avail$statistic_id[data_avail$`Data Type` == "Daily"])
  
})

observeEvent(input$example_data,{
  
  clear_data()
  
  rawData_data$example_data <- TRUE
  rawData_data$daily_data <- HASP::L2701_example_data$Daily
  rawData_data$gwl_data <- HASP::L2701_example_data$Discrete
  rawData_data$qw_data <- HASP::L2701_example_data$QW

  rawData_data$p_code_dv <-  pcode_info[pcode_info$parameter_code == "62610", ]
  
  rawData_data$stat_cd <- "00001"
  rawData_data$p_code_qw <- unique(HASP::L2701_example_data$QW$CharacteristicName)

  rawData_data$available_data <- data_available("USGS-263819081585801")
  rawData_data$site_meta <- site_summary("USGS-263819081585801")
  
  updateTextInput(session, "siteID", value = "USGS-263819081585801")
  
  shinyAce::updateAceEditor(session, 
                            editorId = "get_data_code", 
                            value = setup() )
})

observeEvent(input$get_data_qw, {
  
  rawData_data$example_data <- FALSE
  
  site_id <- input$siteID
  site_info <- site_summary(site_id)

  showNotification("Loading QW", 
                   duration = NULL, id = "load3")
  
  qw_data <- dataRetrieval::readWQPqw(paste0("USGS-", site_id), 
                                                   parameterCd = "")
  
  rawData_data$qw_data <- qw_data
  rawData_data$p_code_qw <- unique(qw_data$CharacteristicName)
  
  removeNotification(id = "load3")

})

observeEvent(input$get_data_dv, {

  rawData_data$example_data <- FALSE
  
  site_id <- input$siteID

  shinyAce::updateAceEditor(session, 
                            editorId = "get_data_code", 
                            value = setup() )
  
  if(!any(grepl("Daily", rawData_data$available_data$`Data Type`))) {
    showNotification("This site doesn't have any daily data available",
                     type = "error")
    rawData_data$daily_data <- NULL
  } else {
    showNotification("Loading Daily Groundwater Data", 
                     duration = NULL, id = "load")

    rawData_data$daily_data <- dataRetrieval::read_waterdata_daily(monitoring_location_id = site_id, 
                                                                   parameter_code = input$pcode, 
                                                                   statistic_id = input$statcd, 
                                                                   skipGeometry = TRUE)
    
    
    removeNotification(id = "load")
  }
    
})

observeEvent(input$get_data_ground, {
  rawData_data$example_data <- FALSE
  
  site_id <- input$siteID

  site_info <- site_summary(site_id)

  if(!any(grepl("GW", site_info$site_type_code))){
    showNotification("The site is not identified as a groundwater site.", 
                     type = "error")
  }
 
  shinyAce::updateAceEditor(session, 
                            editorId = "get_data_code", 
                            value = setup() )
  
  showNotification("Loading Discrete Groundwater Data", 
                   duration = NULL, id = "load2")
  
  rawData_data$gwl_data <- dataRetrieval::read_waterdata_field_measurements(monitoring_location_id = site_id, 
                                                                            skipGeometry = TRUE)
  
  removeNotification(id = "load2")

})

dvData <- reactive({
  return(rawData_data$daily_data)
})

qwData <- reactive({
  return(rawData_data$qw_data)
})

gwlData <- reactive({
  return(rawData_data$gwl_data)
})

availData <- reactive({
  return(rawData_data$available_data)
})

siteData <- reactive({
  return(rawData_data$site_meta)
})

p_code_qw <- reactive({
  return(rawData_data$p_code_qw)
})

observe({
  updateRadioButtons(session, "pcode_plot", 
                            choices = p_code_qw(), selected = p_code_qw()[1])
})

p_code_dv <- reactive({
  return(rawData_data$p_code_dv)
})

stat_cd <- reactive({
  return(rawData_data$stat_cd)
})

observe({
  choices_dv <- p_code_dv()

  updateRadioButtons(session, inputId = "pcode",choiceNames = choices_dv$parameter_name,
                    choiceValues = choices_dv$parameter_code, selected = choices_dv$parameter_code[1])
})

observe({
  choices_st <- stat_cd()
  
  updateRadioButtons(session, inputId = "statcd",
                     choices = choices_st, selected = choices_st[1])
})


