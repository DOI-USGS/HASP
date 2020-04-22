gwl_plot <- reactive({
  
  validate(
    need(!is.null(rawData_data$daily_data), "Please select a data set")
  )
  
  p_code_dv <- input$pcode 
  val_col <- input$gwl_vals
  plot_title <- paste(attr(dvData(), "siteInfo")[["station_nm"]],
                      attr(dvData(), "siteInfo")[["site_no"]], sep = "\n")
  
  gwl_plot <-  gwl_plot_all(dvData(), gwlData(), 
                            p_code_dv = p_code_dv,
                            plot_title = plot_title,
                            value_col = val_col,
                            add_trend = TRUE) 
  
  
  return(gwl_plot)
  
})

gwl_table <- reactive({
  
  p_code_dv <- input$pcode 
  stat_cd <- input$statcd
  
  validate(
    need(!is.null(rawData_data$daily_data), "Please select a data set")
  )

  val_col <- paste("X", p_code_dv, stat_cd, sep = "_")
  gwl_tab <-  site_data_summary(dvData(), 
                                sum_col = val_col) 
  
  gwl_tab <- gwl_tab[,-1]
  names(gwl_tab) <- gsub("_site", "", names(gwl_tab))
  
  gwl_tab_t <- data.frame(t(gwl_tab))
  
  gwl_tab_DT <- DT::datatable(gwl_tab_t,
                              extensions = 'Buttons',
                              colnames = '',
                              options = list(dom = 'tB',
                                             ordering = FALSE,
                                             buttons = c('csv'))) %>% 
    DT::formatSignif(1, digits = 3)
  
  return(gwl_tab_DT)
  
})

gwl_plot_out <- reactive({
  p_code_dv <- input$pcode 
  stat_cd <- input$statcd
  
  val_col <- input$gwl_vals
  
  sum_col <- paste("X", p_code_dv, stat_cd, sep = "_")
  code_out <- paste0(setup(),'
val_col <- "', val_col,'"
gwl_plot <-  gwl_plot_all(gw_level_dv, gwl_data, 
                          p_code_dv = p_code_dv,
                          val_col = val_col,
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
    need(!is.null(rawData_data$daily_data), "Please select a daily data set")
  )
  
  p_code_dv <- input$pcode 
  stat_cd <- input$statcd
  
  value_col <- paste("X", p_code_dv, stat_cd, sep = "_")
  approv_col <- paste0(value_col, "_cd")
  
  plot_title <- paste(attr(dvData(), "siteInfo")[["station_nm"]],
                      attr(dvData(), "siteInfo")[["site_no"]], sep = "\n")
  week_plot <-  weekly_frequency_plot(dvData(), 
                                      date_col = "Date",
                                      value_col = value_col,
                                      approved_col = approv_col,
                                      plot_title = plot_title) 
  
  return(week_plot)
  
})

week_table_df <- reactive({
  
  validate(
    need(!is.null(rawData_data$daily_data), "Please select a daily data set")
  )
  
  p_code_dv <- input$pcode 
  stat_cd <- input$statcd
  value_col <- paste("X", p_code_dv, stat_cd, sep = "_")
  approv_col <- paste0(value_col, "_cd")
  
  week_tab <-  weekly_frequency_table(dvData(), 
                                      date_col = "Date",
                                      value_col = value_col,
                                      approved_col = approv_col) %>% 
    select("Week" = week,
           "Lowest<br>median" = minMed,
           "10th" = p10,
           "25th" = p25,
           "50th" = p50,
           "75th" = p75,
           "90th" = p90,
           "Highest<br>median" = maxMed,
           "# Years" = nYears)
  return(week_tab)
})

week_table <- reactive({
  
  week_tab_DT <- DT::datatable(week_table_df(), 
                               rownames = FALSE, escape = FALSE,
                               options = list(dom = 'tp')) %>% 
    DT::formatSignif(c(-1, -9), digits = 3)
  
  return(week_tab_DT)
  
})

week_plot_out <- reactive({
  code_out <- paste0(setup(),'

week_plot <-  weekly_frequency_plot(gw_level_dv, 
                                    p_code_dv, stat_cd, 
                                    plot_title = plot_title)
week_plot

week_table <- weekly_frequency_table(gw_level_dv,
                                     p_code_dv, 
                                     statCd = stat_cd, 
                                     date_col = "Date")

# To save:
# Fiddle with height and width (in inches) for best results:
# Change file name extension to save as png.
# ggplot2::ggsave(week_plot, file="week_plot.pdf",
#                        height = 9,
#                        width = 11)
  ')
  code_out
})

year2_plot <- reactive({
  
  validate(
    need(!is.null(rawData_data$daily_data), "Please select a daily data set")
  )
  p_code_dv <- input$pcode 
  stat_cd <- input$statcd
  
  value_col <- paste("X", p_code_dv, stat_cd, sep = "_")
  approv_col <- paste0(value_col, "_cd")
  
  plot_title <- paste(attr(dvData(), "siteInfo")[["station_nm"]],
                      attr(dvData(), "siteInfo")[["site_no"]], sep = "\n")
  year2_graph <-  daily_gwl_2yr_plot(dvData(), 
                                     date_col = "Date",
                                     value_col = value_col,
                                     approved_col = approv_col,
                                     plot_title = plot_title,
                                     historical_stat = "mean",
                                     month_breaks = TRUE)
 
  
  
  return(year2_graph)
  
})

year2_table_df <- reactive({
  validate(
    need(!is.null(rawData_data$daily_data), "Please select a data set")
  )
  p_code_dv <- input$pcode 
  stat_cd <- input$statcd
  value_col <- paste("X", p_code_dv, stat_cd, sep = "_")
  approv_col <- paste0(value_col, "_cd")
  daily_tab <-  daily_frequency_table(dvData(),
                                      date_col = "Date",
                                      value_col = value_col,
                                      approved_col = approv_col) %>%
    rename("DOY" = DOY,
           "Maximum" = max,
           "Mean" = mean,
           "Minimum" = min,
           "# Points" = points)
  return(daily_tab)
})

year2_table <- reactive({
  
  daily_tab_DT <- DT::datatable(year2_table_df(), 
                                rownames = FALSE,
                               options = list(dom = 'tp')) %>% 
    DT::formatSignif(c(2:4), digits = 3)
  
  return(daily_tab_DT)
  
})




year2_plot_out <- reactive({
  code_out <- paste0(setup(),'

year2_plot <-  daily_gwl_2yr_plot(gw_level_dv, 
                                  p_code_dv, stat_cd, 
                                  plot_title = plot_title,
                                  historical_stat = "mean",
                                  month_breaks = TRUE)
year2_plot

daily_frequencies <- daily_frequency_table(gw_level_dv,
                                           p_code_dv, stat_cd)
# To save:
# Fiddle with height and width (in inches) for best results:
# Change file name extension to save as png.
# ggplot2::ggsave(year2_plot, file="year2_plot.pdf",
#                        height = 9,
#                        width = 11)
  ')
  code_out
})

month_plot <- reactive({
  
  validate(
    need(!is.null(rawData_data$daily_data), "Please select a data set")
  )
  p_code_dv <- input$pcode 
  stat_cd <- input$statcd
  value_col <- paste("X", p_code_dv, stat_cd, sep = "_")
  approv_col <- paste0(value_col, "_cd")
  
  plot_title <- paste(attr(dvData(), "siteInfo")[["station_nm"]],
                      attr(dvData(), "siteInfo")[["site_no"]], sep = "\n")
  
  month_plot <-  monthly_frequency_plot(dvData(), 
                                        date_col = "Date",
                                        value_col = value_col,
                                        approved_col = approv_col,
                                        plot_title = plot_title) 
  
  
  return(month_plot)
  
})

month_table_df <- reactive({
  validate(
    need(!is.null(rawData_data$daily_data), "Please select a data set")
  )
  p_code_dv <- input$pcode 
  stat_cd <- input$statcd
  value_col <- paste("X", p_code_dv, stat_cd, sep = "_")
  approv_col <- paste0(value_col, "_cd")
  
  month_tab <-  monthly_frequency_table(dvData(), 
                                        date_col = "Date",
                                        value_col = value_col,
                                        approved_col = approv_col) %>%
    select(month, minMed, p25, p50, p75, maxMed, nYears) %>%
    mutate(month = month.abb[month]) %>%
    rename("Month" = month,
           "Lowest<br>median" = minMed,
           "25th" = p25,
           "50th" = p50,
           "75th" = p75,
           "Highest<br>median" = maxMed,
           "Number<br>of years" = nYears)
  return(month_tab)
})

month_table <- reactive({
  
  month_tab_DT <- DT::datatable(month_table_df(), 
                                rownames = FALSE, escape = FALSE,
                                options = list(dom = 't',
                                               pageLength = 12)) %>% 
    DT::formatSignif(c(2:6), digits = 3)
  
  return(month_tab_DT)
  
})

month_plot_out <- reactive({
  val_col <- input$gwl_vals
  
  code_out <- paste0(setup(),'
val_col <- "', val_col,'"

month_plot <-  monthly_frequency_plot(gwl_data, 
                                      value_col = val_col,
                                      plot_title = plot_title)
month_plot

month_frequencies <- monthly_frequency_table(gwl_data,
                                             value_col = val_col)
# To save:
# Fiddle with height and width (in inches) for best results:
# Change file name extension to save as png.
# ggplot2::ggsave(month_plot, file="month_plot.pdf",
#                        height = 9,
#                        width = 11)
  ')
  code_out
})