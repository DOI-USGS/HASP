rawData_data <- reactiveValues(daily_data = NULL,
                               example_data = FALSE, 
                               daily_data = NULL,
                               gwl_data = NULL,
                               qw_data = NULL,
                               p_code_dv = "62610",
                               stat_cd = "00001",
                               available_data = NULL,
                               site_meta = NULL,
                               p_code_qw = NULL)

observeEvent(input$example_data,{
  rawData_data$example_data <- TRUE
  rawData_data$daily_data <- HASP::L2701_example_data$Daily
  rawData_data$gwl_data <- HASP::L2701_example_data$Discrete
  rawData_data$qw_data <- HASP::L2701_example_data$QW
  rawData_data$p_code_dv <- "62610"
  rawData_data$stat_cd <- "00001"
  rawData_data$p_code_qw <- c("00095","90095","00940","99220")

  rawData_data$available_data <- data_available("263819081585801")
  rawData_data$site_meta <- site_summary("263819081585801")
  
  updateTextInput(session, "siteID", value = "263819081585801")
  
  shinyAce::updateAceEditor(session, 
                            editorId = "get_data_code", 
                            value = setup() )
  
})

observeEvent(input$get_data,{
  rawData_data$example_data <- FALSE
  
  site_id <- input$siteID
  parameter_cd <- input$pcode
  stat_cd <- input$statcd
  
  rawData_data$available_data <- data_available(site_id)
  
  site_info <- site_summary(site_id)

  if(!any(grepl("GW", site_info$site_tp_cd))){
    showNotification("The site is not identified as a groundwater site.", 
                     type = "error")
  }
  
  rawData_data$site_meta <-  site_info
  
  pcodes_qw <- dataRetrieval::whatNWISdata(siteNumber = site_id, service = "qw") %>% 
    filter(!is.na(parm_cd)) %>% 
    pull(parm_cd)
  
  rawData_data$p_code_qw <- pcodes_qw
  
  shinyAce::updateAceEditor(session, 
                            editorId = "get_data_code", 
                            value = setup() )
  
  showNotification("Loading Daily", 
                   duration = NULL, id = "load")

  rawData_data$daily_data <- dataRetrieval::readNWISdv(site_id, 
                                                       parameter_cd, 
                                                       statCd = stat_cd)

  
  removeNotification(id = "load")
  
  showNotification("Loading Discrete", 
                   duration = NULL, id = "load2")
  
  rawData_data$gwl_data <- dataRetrieval::readNWISgwl(site_id)
  
  removeNotification(id = "load2")
  
  showNotification("Loading QW", 
                   duration = NULL, id = "load3")
  
  rawData_data$qw_data <- dataRetrieval::readNWISqw(site_id, 
                                                    p_code_qw())

  removeNotification(id = "load3")
  
  rawData_data$p_code_dv <- input$pcode
  rawData_data$stat_cd <- input$statcd
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
  updateCheckboxGroupInput(session, "pcode_plot", 
                            choices = p_code_qw(), selected = p_code_qw())
})

observeEvent(input$get_data_avail,{
  site_id <- input$siteID

  site_info <- site_summary(site_id)
  
  if(!any(grepl("GW", site_info$site_tp_cd))){
    showNotification("The site is not identified as a groundwater site.", 
                     type = "error")
  }
  
  rawData_data$available_data <- data_available(site_id)
  rawData_data$site_meta <-  site_info
  
  pcodes_qw <- dataRetrieval::whatNWISdata(siteNumber = site_id, service = "qw") %>% 
    filter(!is.na(parm_cd)) %>% 
    pull(parm_cd)
  
  rawData_data$p_code_qw <- pcodes_qw
  
})