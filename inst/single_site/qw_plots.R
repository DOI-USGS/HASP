cl_trend_plot <- reactive({
  
  validate(
    need(!is.null(rawData_data$qw_data), "Please select a data set")
  )
  plot_title <- paste(attr(qwData(), "siteInfo")[["station_nm"]],
                      attr(qwData(), "siteInfo")[["site_no"]], sep = "\n")
  chl_plot <-  trend_plot(qwData(), plot_title = plot_title) 
  
  
  return(chl_plot)
  
})

cl_trend_table <- reactive({
  
  validate(
    need(!is.null(rawData_data$qw_data), "Please select a data set")
  )
  
  qw_table <-  kendell_test_5_20_years(dplyr::filter(qw_data, 
                                                    parm_cd %in% c("00940","99220")), 
                                      seasonal = TRUE,
                                      enough_5 = 1,
                                      enough_20 = 1,
                                      date_col = "sample_dt",
                                      value_col = "result_va")
  
  qw_table_DT <- DT::datatable(qw_table, 
                               rownames = FALSE,
                               extensions = 'Buttons',
                               options = list(dom = 'tB',
                                             ordering = FALSE,
                                             buttons = c('csv'))) %>% 
    formatSignif(2:3, digits = 2)
  
  return(qw_table_DT)
  
})

cl_trend_plot_out <- reactive({
  code_out <- paste0(setup(),'

cl_trend <-  trend_plot(qw_data,
                        pcode = c("00940","99220"),
                        norm_range = c(225,999), 
                        plot_title = plot_title)
cl_trend

kendell_test_5_20_years(dplyr::filter(qw_data, 
                                      parm_cd %in% c("00940","99220")), 
                        seasonal = TRUE,
                        enough_5 = 1,
                        enough_20 = 1,
                        date_col = "sample_dt",
                        value_col = "result_va")

qw_summary(qw_data, 
           pcode = c("00940","99220"),
           norm_range = c(225,999))

# To save:
# Fiddle with height and width (in inches) for best results:
# Change file name extension to save as png.
# ggplot2::ggsave(cl_trend, file="cl_trend.pdf",
#                        height = 9,
#                        width = 11)
  ')
  code_out
})