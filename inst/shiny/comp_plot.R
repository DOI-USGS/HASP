comp_plot <- reactive({
  
  validate(
    need(!is.null(rawData_data$data), "Please select a data set")
  )
  
  df <- rawData()
  pcode <- input$pcode

  comp_plot <-  plot_composite_data(df, parameter_cd = pcode,
                                    plot_title = rawData_data$aquifer_cd) 
  
  return(comp_plot)
  
})

comp_plot_out <- reactive({
  code_out <- paste0(setup(),'

comp_plot <-  plot_composite_data(aquifer_data, 
                                  parameter_cd = "', input$pcode,'",
                                  plot_title = "',rawData_data$aquifer_cd,'")
comp_plot
# To save:
# Fiddle with height and width (in inches) for best results:
# Change file name extension to save as png.
# ggplot2::ggsave(comp_plot, file="comp_plot.pdf",
#                        height = 9,
#                        width = 11)
  ')
  code_out
})