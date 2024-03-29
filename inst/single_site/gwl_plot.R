col_stuff <- reactive({

  validate(
    need(!is.null(rawData_data$daily_data) | 
           !is.null(rawData_data$gwl_data),
         "Please select a data set")
  )
  
  p_code_dv <-  input$pcode
  val_col_per <- input$gwl_vals
  
  dv_data <- dvData()
  gwl_data <- gwlData()

  includes_gwl <- !is.null(gwl_data) 
  includes_dv <- !is.null(dv_data)
  includes_both <- includes_gwl & includes_dv
  
  y_label <- dataRetrieval::readNWISpCode(input$pcode)$parameter_nm
  
  if(includes_both){
    date_col = c("Date", "lev_dt")
    value_col = c(paste("X",  input$pcode, input$statcd, sep = "_"),
                  input$gwl_vals)
    approved_col = c(paste("X",  input$pcode, input$statcd, "cd", sep = "_"),
                     "lev_age_cd") 
    
  } else if(includes_dv){
    date_col = "Date"
    value_col = paste("X",  input$pcode, input$statcd, sep = "_")
    approved_col = paste("X",  input$pcode, input$statcd, "cd", sep = "_")
  } else if(includes_gwl){
    date_col = "lev_dt"
    value_col = input$gwl_vals
    approved_col = "lev_age_cd" 
    if("sl_datum_cd" %in% names(gwl_data)){
      datum <- unique(gwl_data$sl_datum_cd)
      y_label <- sprintf("Elevation above %s, feet", datum)
    } else {
      y_label <- "Elevation"
    }
    
  }
  
  return(list(date_col = date_col,
              value_col = value_col,
              approved_col = approved_col,
              y_label = y_label,
              includes_both = includes_both))
})

gwl_plot <- reactive({

  validate(
    need(!is.null(rawData_data$daily_data) | 
           !is.null(rawData_data$gwl_data),
         "Please select a data set")
  )
  
  dv_data <- dvData()
  gwl_data <- gwlData()
  p_code_dv <- input$pcode
  
  columns <- col_stuff()

  plot_title <- paste(attr(dv_data, "siteInfo")[["station_nm"]],
                      attr(dv_data, "siteInfo")[["site_no"]], sep = "\n")

  gwl_plot <-  gwl_plot_all(dv_data, 
                            gwl_data, 
                            parameter_cd = p_code_dv, 
                            plot_title = plot_title,
                            y_label = columns$y_label,
                            add_trend = TRUE, 
                            flip = input$flip_plot) 
  
  return(gwl_plot)
  
})

gwl_table <- reactive({
  
  validate(
    need(!is.null(rawData_data$daily_data),
         "Please select a daily data set")
  )
  
  columns <- col_stuff()

  val_col <- columns$value_col[1]
  gwl_tab <-  site_data_summary(dvData(),
                                value_col = val_col,
                                site_col = "site_no") 
  
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
  columns <- col_stuff() 
  
  val_col <- columns$value_col
  date_col <- columns$date_col
  approved_col <- columns$approved_col
  y_label <- columns$y_label

  code_out <- paste0(setup(),'
y_label <- "', y_label,'"
flip <- ', input$flip_plot,'
gwl_plot <-  gwl_plot_all(gw_level_dv, 
                          gwl_data, 
                          parameter_cd = "', p_code_dv,'",
                          stat_cd = "', input$statcd,'",
                          y_label = y_label,
                          plot_title = plot_title,
                          flip = flip,
                          add_trend = TRUE)
gwl_plot

df <- gw_level_dv
names(df)[names(df) == val_col] <- "value"
gwl_summary_table <- site_data_summary(df)

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
  
  dv_data <- dvData()
  gwl_data <- gwlData()

  plot_title <- paste(attr(dv_data, "siteInfo")[["station_nm"]],
                      attr(dv_data, "siteInfo")[["site_no"]], sep = "\n")
  
  y_label <- dataRetrieval::readNWISpCode(p_code_dv)$parameter_nm
  
    week_plot <-  weekly_frequency_plot(dv_data, 
                                        gwl_data,
                                        parameter_cd = p_code_dv,
                                        stat_cd = stat_cd,
                                        plot_title = plot_title, 
                                        y_axis_label = y_label,
                                        flip = input$flip_plot) 
  
  return(week_plot)
  
})

week_table_df <- reactive({
  
  validate(
    need(!is.null(rawData_data$daily_data), "Please select a daily data set")
  )
  
  p_code_dv <- input$pcode
  stat_cd <- input$statcd

  week_tab <-  weekly_frequency_table(dvData(), gwlData(),
                                      parameter_cd = p_code_dv,
                                      stat_cd = stat_cd,
                                      flip = input$flip_plot) %>% 
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
  
  p_code_dv <- input$pcode
  stat_cd <- input$statcd

  code_out <- paste0(setup(),'
flip <- ', input$flip_plot,'

week_plot <-  weekly_frequency_plot(gw_level_dv,  
                                    gwl_data,
                                    parameter_cd = "', p_code_dv,'",
                                    stat_cd = "', stat_cd, '"
                                    plot_title = plot_title,
                                    flip = flip)
week_plot

week_table <- weekly_frequency_table(gw_level_dv,
                                     gwl_data,
                                     parameter_cd = "', p_code_dv,'",
                                     stat_cd = "', stat_cd, '"
                                     flip = flip)

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

  plot_title <- paste(attr(dvData(), "siteInfo")[["station_nm"]],
                      attr(dvData(), "siteInfo")[["site_no"]], sep = "\n")
  y_label <- dataRetrieval::readNWISpCode(p_code_dv)$parameter_nm
  year2_graph <-  daily_gwl_plot(dvData(), 
                                 gwlData(),
                                 parameter_cd = p_code_dv,
                                 stat_cd = stat_cd,
                                 plot_title = plot_title,
                                 historical_stat = "mean",
                                 month_breaks = TRUE, 
                                 y_axis_label = y_label,
                                 flip = input$flip_plot)
 
  
  
  return(year2_graph)
  
})

year2_table_df <- reactive({
  validate(
    need(!is.null(rawData_data$daily_data), "Please select a data set")
  )
  p_code_dv <- input$pcode
  stat_cd <- input$statcd

  daily_tab <-  daily_frequency_table(dvData(),
                                      gwlData(),
                                      parameter_cd = p_code_dv,
                                      stat_cd = stat_cd) %>%
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
  
  p_code_dv <- input$pcode
  stat_cd <- input$statcd

  code_out <- paste0(setup(),'
flip <- ', input$flip_plot, '

year2_plot <-  daily_gwl_plot(gw_level_dv, 
                              gwl_data,
                              parameter_cd = "', p_code_dv, '",
                              stat_cd = "', stat_cd, '"
                              plot_title = plot_title,
                              historical_stat = "mean",
                              flip = flip,
                              month_breaks = TRUE)
year2_plot

daily_frequencies <- daily_frequency_table(gw_level_dv,
                                           gwl_data,
                                           parameter_cd = "', p_code_dv, '"
                                           stat_cd = "', stat_cd, '")
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
  p_code_dv <-  input$pcode
  stat_cd <- input$statcd

  plot_title <- paste(attr(dvData(), "siteInfo")[["station_nm"]],
                      attr(dvData(), "siteInfo")[["site_no"]], sep = "\n")
  
  y_label <- dataRetrieval::readNWISpCode(p_code_dv)$parameter_nm
  
  month_plot <-  monthly_frequency_plot(dvData(), 
                                        gwlData(),
                                        parameter_cd = p_code_dv,
                                        stat_cd = stat_cd,
                                        plot_title = plot_title, 
                                        y_axis_label = y_label,
                                        flip = input$flip_plot)
  
  return(month_plot)
  
})

month_table_df <- reactive({
  validate(
    need(!is.null(rawData_data$daily_data), "Please select a data set")
  )
  p_code_dv <- input$pcode 
  stat_cd <- input$statcd

  month_tab <-  monthly_frequency_table(dvData(), 
                                        gwlData(),
                                        parameter_cd = p_code_dv,
                                        stat_cd = stat_cd,
                                        flip = input$flip_plot) %>%
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
  
  p_code_dv <- input$pcode
  stat_cd <- input$statcd

  code_out <- paste0(setup(),'
flip <- ', input$flip_plot, '

month_plot <-  monthly_frequency_plot(gw_level_dv,  
                                      gwl_data,
                                      parameter_cd = "', p_code_dv,'"
                                      stat_cd = "', stat_cd, '"
                                      plot_title = plot_title,
                                      flip = flip)
month_plot

month_frequencies <- monthly_frequency_table(gw_level_dv,  
                                             gwl_data,
                                             parameter_cd = "', p_code_dv,'"
                                             stat_cd = "', stat_cd, '"
                                             flip = flip)
# To save:
# Fiddle with height and width (in inches) for best results:
# Change file name extension to save as png.
# ggplot2::ggsave(month_plot, file="month_plot.pdf",
#                        height = 9,
#                        width = 11)
  ')
  code_out
})