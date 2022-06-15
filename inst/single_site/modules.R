
ggraph_table_downloaders <- function(id, init_text) {
  ns <- NS(id)
  
  tagList(
    shinycssloaders::withSpinner(plotOutput(ns('plot'))),
    fluidRow(
      column(3, downloadButton(ns('download_plot'), 'Download png')),
      column(3, downloadButton(ns('download_xlsx'), 'Download plot data'))
    ),
    DT::dataTableOutput(ns('table')),
    downloadButton(ns('download_table'), 'Download full table'),
    h4("R Code:"),
    shinyAce::aceEditor(outputId = ns('code'), value = init_text, 
                        mode = "r", theme = "chrome", readOnly = TRUE)
  )
}

ggraph_table_downloaders_1line <- function(id, init_text) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(6,  shinycssloaders::withSpinner(plotOutput(ns('plot')))),
      column(6, DT::dataTableOutput(ns('table')))
    ),
    fluidRow(
      column(3, downloadButton(ns('download_plot'), 'Download png')),
      column(3, downloadButton(ns('download_xlsx'), 'Download plot data'))
    ),
    h4("R Code:"),
    shinyAce::aceEditor(outputId = ns('code'), value = init_text, 
                        mode = "r", theme = "chrome", readOnly = TRUE)
  )
}



graph_table_download_code <- function(input, output, session, 
                                      plot_gg, table_DT,
                                      code_out, raw_data, 
                                      table_df = NULL){
  
  ns <- session$ns
  
  output$plot <- renderPlot({
    
    validate(
      need((raw_data()), "Please select a data set")
    )
    
    shinyAce::updateAceEditor(session, editorId = "code", value = code_out() )
    
    plot_gg()    
  })
  
  output$table <- DT::renderDataTable({
    table_DT()
  }, server = FALSE)
  
  output$download_plot <- downloadHandler(
    
    filename = "plot.png",
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_gg(),
                      device = "png", width = 11,
                      height = 9)
    }
  )
  
  output$download_xlsx <- downloadHandler(
    
    filename = "plot_data.xlsx",
    
    content = function(file) {
      
      x <- plot_gg()[['layers']]
      
      wb <- openxlsx::createWorkbook()
      
      for(i in seq_len(length(x))){
        
        data_i <- x[[i]][["data"]]
        sheet_name <- paste("Sheet", i)
        openxlsx::addWorksheet(wb, sheetName = sheet_name)
        
        openxlsx::writeData(wb,
                            sheet = sheet_name, 
                            x = data_i)
      }
      openxlsx::saveWorkbook(wb, 
                   file = file, 
                   overwrite = TRUE)
    }
  )
  
  output$download_table <- downloadHandler(
    
    filename = "hasp_data.csv",
    content = function(file) {
      write.csv(table_df(), file, row.names = FALSE)
    }
  )
  
}
