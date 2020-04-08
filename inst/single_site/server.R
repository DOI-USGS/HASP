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
  
  output$dataAvailable <- DT::renderDataTable({
      availData()
    }, escape=FALSE, rownames = FALSE, options = list(dom = "t"))
  
  source("get_data.R",local=TRUE)$value
  source("gwl_plot.R",local=TRUE)$value
  source("qw_plots.R",local=TRUE)$value
  
  callModule(graph_table_download_code, 'gwl_graph', 
             plot_gg = gwl_plot, 
             table_DT = gwl_table,
             code_out = gwl_plot_out, 
             raw_data = reactive({rawData_data$daily_data}))
  
  callModule(graph_table_download_code, 'week_graph', 
             plot_gg = week_plot, 
             table_DT = week_table,
             code_out = week_plot_out, 
             table_df = week_table_df,
             raw_data = reactive({rawData_data$daily_data}))
  
  callModule(graph_table_download_code, 'month_graph', 
             plot_gg = month_plot, 
             table_DT = month_table,
             code_out = month_plot_out, 
             raw_data = reactive({rawData_data$daily_data}))
  
  callModule(graph_table_download_code, 'year2_graph', 
             plot_gg = year2_plot, 
             table_DT = year2_table,
             code_out = year2_plot_out, 
             table_df = year2_table_df,
             raw_data = reactive({rawData_data$daily_data}))
  
  callModule(graph_table_download_code, 'chloride_graph', 
             plot_gg = cl_trend_plot, 
             table_DT = cl_trend_table,
             code_out = cl_trend_plot_out, 
             raw_data = reactive({rawData_data$qw_data}))
  
  callModule(graph_table_download_code, 'qw_graph', 
             plot_gg = qw1_plot, 
             table_DT = qw1_table,
             code_out = qw1_plot_out, 
             raw_data = reactive({rawData_data$qw_data}))
  
  
  callModule(graph_table_download_code, 'ch_sc_graph', 
             plot_gg = cl_sc_plot, 
             table_DT = cl_sc_table,
             table_df = cl_sc_table_df,
             code_out = cl_sc_plot_out, 
             raw_data = reactive({rawData_data$qw_data}))
  
  setup <- reactive({
    
    p_code <- rawData_data$p_code_dv
    site_id <- input$siteID
    stat_cd <- input$statcd
    pcodeqw <- input$pcode_plot
    
    if(rawData_data$example_data){
      setup_code <- paste0('library(HASP)

gw_level_dv <- L2701_example_data$Daily
gwl_data <- L2701_example_data$Discrete
qw_data <- L2701_example_data$QW

p_code_dv <- "62610"
stat_cd <- "00001"')

    } else {
      setup_code <- paste0('library(HASP)
library(dataRetrieval)

site_id <- "', site_id ,'"
p_code_dv <- c("', paste(p_code, collapse = '", "') ,'")
stat_cd <- "00001"

gw_level_dv <- readNWISdv(site_id = site_id,
                           parameterCd = "', p_code,'",
                           statCd = "', stat_cd, '")
gwl_data <- readNWISqwl(site_id = site_id)

plot_title <- paste(attr(gwl_data, "siteInfo")[["station_nm"]],
                    site_id, sep = "\\\\n")') 
      
      if(!isTRUE(is.null(rawData_data$qw_data))){
        setup_code <- paste0(setup_code,'
pcodes_qw <- c("', paste(pcodeqw, collapse = '", "'),'")
qw_data <- readNWISqw(site_id = site_id,
                      parameterCd = pcodes_qw)
')
      }      
    }
    
    setup_code
  })
  

  
  session$onSessionEnded(stopApp)
  
})
