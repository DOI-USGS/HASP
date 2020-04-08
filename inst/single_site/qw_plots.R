cl_trend_plot <- reactive({
  
  validate(
    need(!is.null(rawData_data$qw_data), "Click the 'Get QW Data' button")
  )
  plot_title <- paste(attr(qwData(), "siteInfo")[["station_nm"]],
                      attr(qwData(), "siteInfo")[["site_no"]], sep = "\n")
  chl_plot <-  trend_plot(qwData(), plot_title = plot_title) 
  
  
  return(chl_plot)
  
})

cl_trend_table <- reactive({
  
  validate(
    need(!is.null(rawData_data$qw_data), "Click the 'Get QW Data' button")
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

cl_sc_plot <- reactive({
  
  validate(
    need(!is.null(rawData_data$qw_data), "Click the 'Get QW Data' button")
  )
  plot_title <- paste(attr(qwData(), "siteInfo")[["station_nm"]],
                      attr(qwData(), "siteInfo")[["site_no"]], sep = "\n")
  Sc_Cl <-  Sc_Cl_plot(qwData(), plot_title = plot_title) 
  
  return(Sc_Cl)
  
})

cl_sc_table_df <- reactive({
  validate(
    need(!is.null(rawData_data$qw_data), "Please select a data set")
  )
  
  sl_cl_table <-  Sc_Cl_table(qwData())
  return(sl_cl_table)
})

cl_sc_table <- reactive({
  
  sl_cl_table_DT <- DT::datatable(cl_sc_table_df(), 
                                  rownames = FALSE,
                                  options = list(dom = 't')) 
  
  return(sl_cl_table_DT)
  
})

cl_sc_plot_out <- reactive({
  code_out <- paste0(setup(),'

Sc_Cl <-  Sc_Cl_plot(qw_data, 
                        plot_title = plot_title)
Sc_Cl

Sc_Cl_df <- Sc_Cl_table(qw_data)

# To save:
# Fiddle with height and width (in inches) for best results:
# Change file name extension to save as png.
# ggplot2::ggsave(Sc_Cl, file="Sc_Cl.pdf",
#                        height = 9,
#                        width = 11)
  ')
  code_out
})


qw1_plot <- reactive({
  
  validate(
    need(!is.null(rawData_data$qw_data), "Click the 'Get QW Data' button")
  )
  
  pcode <- input$pcode_plot
  
  plot_title <- paste(attr(qwData(), "siteInfo")[["station_nm"]],
                      attr(qwData(), "siteInfo")[["site_no"]], sep = "\n")
  qwplot <-  qw_plot(qwData(),
                     pcode = pcode,
                     plot_title = plot_title) 
  
  return(qwplot)
  
})

qw1_table <- reactive({
  
  validate(
    need(!is.null(rawData_data$qw_data), "Click the 'Get QW Data' button")
  )
  
  pcode <- input$pcode_plot
  
  qw_table <-  qw_summary(qwData(), pcode = pcode)
  
  qw_table_DT <- DT::datatable(qw_table, 
                               colnames = "",
                               rownames = FALSE,
                               extensions = 'Buttons',
                               options = list(dom = 'tB',
                                              ordering = FALSE,
                                              buttons = c('csv'))) 
  
  return(qw_table_DT)
  
})

qw1_plot_out <- reactive({
  
  pcode <- input$pcode_plot
  
  code_out <- paste0(setup(),'
qw_pcodes <- c("', paste(pcode, collapse = '", "'), '")
qw_dt_plot <-  qw_plot(qw_data, 
                       plot_title = plot_title)
qw_dt_plot

qw_table <- qw_summary(qw_data, pcode)

# To save:
# Fiddle with height and width (in inches) for best results:
# Change file name extension to save as png.
# ggplot2::ggsave(Sc_Cl, file="Sc_Cl.pdf",
#                        height = 9,
#                        width = 11)
  ')
  code_out
})
