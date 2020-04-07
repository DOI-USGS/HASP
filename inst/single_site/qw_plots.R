cl_trend_plot <- reactive({
  
  validate(
    need(!is.null(rawData_data$qw_data), "Please select a data set")
  )
  plot_title <- paste(attr(qwData(), "siteInfo")[["station_nm"]],
                      attr(qwData(), "siteInfo")[["site_no"]], sep = "\n")
  chl_plot <-  trend_plot(qwData(), plot_title = plot_title) 
  
  
  return(chl_plot)
  
})

cl_trend_plot_out <- reactive({
  code_out <- paste0(setup(),'

cl_trend <-  trend_plot(qw_data, 
                        plot_title = plot_title)
cl_trend
# To save:
# Fiddle with height and width (in inches) for best results:
# Change file name extension to save as png.
# ggplot2::ggsave(cl_trend, file="cl_trend.pdf",
#                        height = 9,
#                        width = 11)
  ')
  code_out
})