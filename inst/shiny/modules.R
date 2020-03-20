
ggraph_w_downloaders <- function(id, init_text) {
  ns <- NS(id)
  
  tagList(
    shinycssloaders::withSpinner(plotOutput(ns('plot'))),
    fluidRow(
      column(3, downloadButton(ns('download_plot'), 'Download PNG')),
      column(3, downloadButton(ns('download_csv'), 'Download CSV'))
    ),
    h4("R Code:"),
    shinyAce::aceEditor(outputId = ns('code'), value = init_text, 
                        mode = "r", theme = "chrome", readOnly = TRUE)
  )
}


graph_download_code <- function(input, output, session, 
                                plot_gg, code_out, raw_data){
  
  ns <- session$ns
  
  output$plot <- renderPlot({
    
    validate(
      need(!is.null(raw_data()), "Please select a data set")
    )
    
    shinyAce::updateAceEditor(session, editorId = "code", value = code_out() )
    
    plot_gg()
    
  })
  
  output$download_plot <- downloadHandler(
    
    filename = "plot.png",
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_gg(),
                      device = "png", width = 11,
                      height = 9)
    }
  )
  
  output$download_csv <- downloadHandler(
    
    filename = "plot_data.csv",
    
    content = function(file) {
      write.csv(plot_gg()[['data']], file, row.names = FALSE)
    }
  )
  
}
