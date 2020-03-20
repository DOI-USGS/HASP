norm_plot <- reactive({
  
  validate(
    need(!is.null(rawData_data$data), "Please select a data set")
  )
  
  norm_plot <-  plot_normalized_data(rawData(), 
                                    "lev_va", 30) +
    ggplot2::ggtitle(rawData_data$aquifer_cd)
  
  return(norm_plot)
  
})

norm_plot_out <- reactive({
  code_out <- paste0(setup(),'
library(ggplot2)
norm_plot <-  plot_normalized_data(aquifer_data, "lev_va", 30) +
    ggtitle("',rawData_data$aquifer_cd,'")
norm_plot
# To save:
# Fiddle with height and width (in inches) for best results:
# Change file name extension to save as png.
# ggplot2::ggsave(comp_plot, file="norm_plot.pdf",
#                        height = 9,
#                        width = 11)
')
  code_out
})