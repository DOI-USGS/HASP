comp_plot <- reactive({
  
  validate(
    need(!is.null(rawData_data$data), "Please select a data set")
  )

  x <- rawData()
  
  if(sum(!is.na(x$lev_va)) > 0 & sum(!is.na(x$sl_lev_va)) > 0){
    showNotification("Warning, both sl_lev_va AND lev_va contain values", 
                     duration = 10)
  }

  x <- filter_sites(x, input$gwl_vals, 30)

  if(nrow(x) == 0){
    showNotification("No data in ", input$gwl_vals)
    
    new_choice <- c("lev_va","sl_lev_va")
    new_choice <- new_choice[new_choice != input$gwl_vals]
    
    updateRadioButtons(session, "gwl_vals", selected = new_choice)
    
  }
  
  comp_plot <-  plot_composite_data(rawData(), 
                                    sum_col = input$gwl_vals, 
                                    num_years = 30) +
    ggplot2::ggtitle(rawData_data$aquifer_cd)
  
  
  return(comp_plot)
  
})

comp_plot_out <- reactive({
  code_out <- paste0(setup(),'
library(ggplot2)
comp_plot <-  plot_composite_data(aquifer_data, sum_col = "lev_va", num_years = 30) +
    ggtitle("',rawData_data$aquifer_cd,'")
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