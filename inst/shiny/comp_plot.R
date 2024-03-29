comp_plot <- reactive({
  
  validate(
    need(!is.null(rawData_data$data), "Please select a data set")
  )
  
  df <- rawData()
  pcode <- input$pcode

  y <- filter_sites(df,
                    parameter_cd = input$pcode, 
                    start_year = input$start_year, 
                    end_year = input$end_year)
  
  if(nrow(y) == 0){
    showNotification("No sites have complete records within the start/end years",
                     duration = 5)
  }
  
  comp_plot <-  plot_composite_data(y,
                                    parameter_cd = pcode,
                                    plot_title = rawData_data$aquifer_cd) 
  
  return(comp_plot)
  
})

comp_plot_out <- reactive({
  code_out <- paste0(setup(),'
  
aquifer_data_filtered <- filter_sites(aquifer_data,
                                      parameter_cd = "', input$pcode, '", 
                                      start_year = ', input$start_year, ', 
                                      end_year = ', input$end_year, ')
                                     
comp_plot <-  plot_composite_data(aquifer_data_filtered, 
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