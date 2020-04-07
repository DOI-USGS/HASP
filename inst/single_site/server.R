library(shiny)
library(HASP)
options(shiny.maxRequestSize = 200 * 1024^2)
library(dplyr)
library(tidyr)

source("modules.R",local=TRUE)

shinyServer(function(input, output, session) {
  
  
  observe({
    if (input$close > 0) shiny::stopApp()    
  })
  
  source("get_data.R",local=TRUE)$value
  source("gwl_plot.R",local=TRUE)$value
  source("qw_plots.R",local=TRUE)$value
  
  callModule(graph_table_download_code, 'gwl_graph', 
             plot_gg = gwl_plot, 
             table_DT = gwl_table,
             code_out = gwl_plot_out, 
             raw_data = reactive({rawData_data$daily_data}))
  
  callModule(graph_download_code, 'week_graph', 
             plot_gg = week_plot, 
             code_out = week_plot_out, 
             raw_data = reactive({rawData_data$daily_data}))
  
  callModule(graph_download_code, 'year2_graph', 
             plot_gg = year2_plot, 
             code_out = year2_plot_out, 
             raw_data = reactive({rawData_data$daily_data}))
  
  callModule(graph_download_code, 'chloride_graph', 
             plot_gg = cl_trend_plot, 
             code_out = cl_trend_plot_out, 
             raw_data = reactive({rawData_data$qw_data}))
  
  setup <- reactive({
    
    p_code <- input$pcode
    site_id <- input$siteID
    stat_cd <- input$statcd
    
    if(rawData_data$example_data){
      setup_code <- paste0('library(HASP)
gw_level_dv <- L2701_example_data$Daily
gwl_data <- L2701_example_data$Discrete
qw_data <- L2701_example_data$QW
p_code <- "62610"
stat_cd <- "00001"')

    } else {
      setup_code <- paste0('library(HASP)
library(dataRetrieval)
site_id <- "', site_id ,'"

pcodes_cl_sc <- c("00095","90095","00940","99220")
gw_level_dv <- readNWISdv(site_id = site_id,
                           parameterCd = "', p_code,'",
                           statCd = "', stat_cd, '")
gwl_data <- readNWISqwl(site_id = site_id)
qw_data <- readNWISqw(site_id = site_id,
                      parameterCd = pcodes_cl_sc)
plot_title <- paste(attr(gwl_data, "siteInfo")[["station_nm"]],
                    site_id, sep = "\\\\n")')     
    }
    
    setup_code
  })
  

  
  session$onSessionEnded(stopApp)
  
})
