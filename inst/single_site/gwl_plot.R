gwl_plot <- reactive({
  
  validate(
    need(!is.null(rawData_data$daily_data), "Please select a data set")
  )
  plot_title <- paste(attr(dvData(), "siteInfo")[["station_nm"]],
                      attr(dvData(), "siteInfo")[["site_no"]], sep = "\n")
  gwl_plot <-  gwl_plot_all(dvData(), plot_title = plot_title,
                            gwlData(), add_trend = TRUE) 
  
  
  return(gwl_plot)
  
})

gwl_table <- reactive({
  
  validate(
    need(!is.null(rawData_data$daily_data), "Please select a data set")
  )

  val_col <- paste("X", rawData_data$p_code, rawData_data$stat_cd, sep = "_")
  gwl_tab <-  site_data_summary(dvData(), 
                                sum_col = val_col) 
  
  gwl_tab <- gwl_tab[,-1]
  names(gwl_tab) <- gsub("_site", "", names(gwl_tab))
  
  gwl_tab_DT <- DT::datatable(gwl_tab, rownames = FALSE,
                              options = list(dom = 't')) %>% 
    formatSignif(-9, digits = 3)
  
  return(gwl_tab_DT)
  
})

gwl_plot_out <- reactive({
  sum_col <- paste("X", rawData_data$p_code, rawData_data$stat_cd, sep = "_")
  code_out <- paste0(setup(),'

gwl_plot <-  gwl_plot_all(gw_level_dv, gwl_data, 
                          plot_title = plot_title,
                          add_trend = TRUE)
gwl_plot

gwl_summary_table <- site_data_summary(gw_level_dv,
                                       sum_col = "', sum_col,'")

# To save plot:
# Fiddle with height and width (in inches) for best results:
# Change file name extension to save as png.
# ggplot2::ggsave(gwl_plot, file="gwl_plot.pdf",
#                        height = 9,
#                        width = 11)
  ')
  code_out
})

week_plot <- reactive({
  
  validate(
    need(!is.null(rawData_data$daily_data), "Please select a data set")
  )
  plot_title <- paste(attr(dvData(), "siteInfo")[["station_nm"]],
                      attr(dvData(), "siteInfo")[["site_no"]], sep = "\n")
  week_plot <-  weekly_frequency_plot(dvData(), plot_title = plot_title,
                                     p_code_dv = rawData_data$p_code, 
                                     statCd = rawData_data$stat_cd) 
  
  
  return(week_plot)
  
})

year2_plot <- reactive({
  
  validate(
    need(!is.null(rawData_data$daily_data), "Please select a data set")
  )
  plot_title <- paste(attr(dvData(), "siteInfo")[["station_nm"]],
                      attr(dvData(), "siteInfo")[["site_no"]], sep = "\n")
  year2_graph <-  daily_gwl_2yr_plot(dvData(), 
                                     p_code_dv = rawData_data$p_code, 
                                     statCd = rawData_data$stat_cd,
                                     plot_title = plot_title,
                                     historical_stat = "mean",
                                     month_breaks = TRUE)
 
  
  
  return(year2_graph)
  
})




week_plot_out <- reactive({
  code_out <- paste0(setup(),'
p_code <- "', rawData_data$p_code,'"
stat_cd <- "', rawData_data$stat_cd,'"
week_plot <-  weekly_frequency_plot(gw_level_dv, 
                                    p_code, stat_cd, 
                                    plot_title = plot_title)
week_plot
# To save:
# Fiddle with height and width (in inches) for best results:
# Change file name extension to save as png.
# ggplot2::ggsave(week_plot, file="week_plot.pdf",
#                        height = 9,
#                        width = 11)
  ')
  code_out
})

year2_plot_out <- reactive({
  code_out <- paste0(setup(),'
p_code <- "', rawData_data$p_code,'"
stat_cd <- "', rawData_data$stat_cd,'"
year2_plot <-  daily_gwl_2yr_plot(gw_level_dv, 
                                    p_code, stat_cd, 
                                    plot_title = plot_title,
                                    historical_stat = "mean",
                                    month_breaks = TRUE)
year2_plot
# To save:
# Fiddle with height and width (in inches) for best results:
# Change file name extension to save as png.
# ggplot2::ggsave(year2_plot, file="year2_plot.pdf",
#                        height = 9,
#                        width = 11)
  ')
  code_out
})