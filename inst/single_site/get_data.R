rawData_data <- reactiveValues(daily_data = NULL,
                               example_data = FALSE,
                               gwl_data = NULL,
                               qw_data = NULL,
                               p_code_dv = "62610",
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

observeEvent(input$get_data_avail,{
  
  clear_data()
  
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

  pcodes_dv <- dataRetrieval::whatNWISdata(siteNumber = site_id, service = "dv") %>% 
    filter(!is.na(parm_cd))

  rawData_data$p_code_dv <- pcodes_dv$parm_cd
  rawData_data$stat_cd <- unique(pcodes_dv$stat_cd)
  
})

observeEvent(input$example_data,{
  
  clear_data()
  
  rawData_data$example_data <- TRUE
  rawData_data$daily_data <- HASP::L2701_example_data$Daily
  rawData_data$gwl_data <- HASP::L2701_example_data$Discrete
  rawData_data$qw_data <- HASP::L2701_example_data$QW
  
  rawData_data$p_code_dv <-  "62610"
  
  rawData_data$stat_cd <- "00001"
  rawData_data$p_code_qw <- c("00095","90095","00940","99220")

  rawData_data$available_data <- data_available("263819081585801")
  rawData_data$site_meta <- site_summary("263819081585801")
  
  updateTextInput(session, "siteID", value = "263819081585801")
  
  shinyAce::updateAceEditor(session, 
                            editorId = "get_data_code", 
                            value = setup() )
})

observeEvent(input$get_data_qw, {
  
  rawData_data$example_data <- FALSE
  
  site_id <- input$siteID
  site_info <- site_summary(site_id)
  
  rawData_data$available_data <- data_available(site_id)
  
  pcodes_qw <- dataRetrieval::whatNWISdata(siteNumber = site_id, service = "qw") %>% 
    filter(!is.na(parm_cd)) %>% 
    pull(parm_cd)
  
  rawData_data$p_code_qw <- pcodes_qw
  
  showNotification("Loading QW", 
                   duration = NULL, id = "load3")
  
  rawData_data$qw_data <- dataRetrieval::readNWISqw(site_id, parameterCd = "All")
  
  removeNotification(id = "load3")

})

observeEvent(input$get_data_dv, {

  rawData_data$example_data <- FALSE
  
  site_id <- input$siteID

  site_info <- site_summary(site_id)
  
  pcodes_dv <- dataRetrieval::whatNWISdata(siteNumber = site_id, service = "dv") %>% 
    filter(!is.na(parm_cd))

  rawData_data$p_code_dv <- pcodes_dv$parm_cd
  rawData_data$stat_cd <- unique(pcodes_dv$stat_cd)
  rawData_data$site_meta <-  site_info
  
  pcodes_qw <- dataRetrieval::whatNWISdata(siteNumber = site_id, service = "qw") %>% 
    filter(!is.na(parm_cd)) %>% 
    pull(parm_cd)
  
  rawData_data$p_code_qw <- pcodes_qw

  shinyAce::updateAceEditor(session, 
                            editorId = "get_data_code", 
                            value = setup() )
  
  showNotification("Loading Daily Groundwater Data", 
                   duration = NULL, id = "load")
  
  rawData_data$daily_data <- dataRetrieval::readNWISdv(site_id, 
                                                       pcodes_dv$parm_cd, 
                                                       statCd = unique(pcodes_dv$stat_cd))
  
  
  removeNotification(id = "load")
    
})

observeEvent(input$get_data_ground, {
  rawData_data$example_data <- FALSE
  
  site_id <- input$siteID

  rawData_data$available_data <- data_available(site_id)
  
  site_info <- site_summary(site_id)

  if(!any(grepl("GW", site_info$site_tp_cd))){
    showNotification("The site is not identified as a groundwater site.", 
                     type = "error")
  }
 
  shinyAce::updateAceEditor(session, 
                            editorId = "get_data_code", 
                            value = setup() )
  
  showNotification("Loading Discrete Groundwater Data", 
                   duration = NULL, id = "load2")
  
  rawData_data$gwl_data <- dataRetrieval::readNWISgwl(site_id)
  
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
  
  updateRadioButtons(session, inputId = "pcode",
                    choices = choices_dv, selected = choices_dv[1])
})

observe({
  choices_st <- stat_cd()
  
  updateRadioButtons(session, inputId = "statcd",
                     choices = choices_st, selected = choices_st[1])
})