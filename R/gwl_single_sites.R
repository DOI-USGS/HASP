#' Single site groundwater level plots and tables
#' 
#' Funtion to create the periodic groundwater data plot.
#' @export
#' @param gwl data frame returned from dataRetrieval::readNWISgwl
#' @param title character
#' @import ggplot2
#' @import dplyr
#' @rdname gwl_periodic
#' 
#' @examples
#' 
#' site <- "263819081585801"
#' gwl_data <- dataRetrieval::readNWISgwl(site)
#' title <- attr(gwl_data, "siteInfo")[["station_nm"]]
#' gwl_plot_periodic(gwl_data, title = title)
gwl_plot_periodic <- function(gwl, title = ""){
  
  if(!all(c("lev_dt", "sl_lev_va", "lev_age_cd", "sl_datum_cd") %in% names(gwl))){
    stop("data frame gwl doesn't include all mandatory columns")
  }
  
  lev_dt <- sl_lev_va <- lev_age_cd <- ".dplyr"
  datum <- unique(gwl$sl_datum_cd)
  y_label <- sprintf("Elevation above %s, feet", datum)
  
  plot_out <- ggplot(data = gwl,
         aes(x = lev_dt, y = sl_lev_va)) +
    geom_line(linetype = "dashed", color = "blue") +
    geom_point(aes(color = lev_age_cd), size = 1) +
    theme_gwl() +
    labs(caption = paste("Plot created:", Sys.Date()), 
         y = y_label, x = "Years") +
    expand_limits(y = 0) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0))) +
    scale_color_manual("",
                       values = c("A" = "blue", "P" = "red"), 
                       labels = c("A" = "Approved water-level measurement",
                                  "P" = "Provisional water-level measurement")) +
    ggtitle(title, 
            subtitle = "U.S. Geological Survey") +
    theme(legend.position = "bottom",
          legend.direction = "vertical",
          legend.title = element_blank())

  return(plot_out)
  
}


#' @rdname gwl_periodic
#' @export
#' @param dv daily value groundwater levels. Must include columns 
#' @examples 
#' site <- "263819081585801"
#' parameterCd <- "62610"
#' statCd <- "00001"
#' dv <- dataRetrieval::readNWISdv(site, parameterCd, statCd = statCd)
#' 
#' gwl_plot_all(dv, gwl_data)
gwl_plot_all <- function(dv, gwl, title = ""){
  
  if(!all(c("lev_dt", "sl_lev_va", "lev_age_cd") %in% names(gwl))){
    stop("data frame gwl doesn't include all mandatory columns")
  }

  if(!all(c("Date") %in% names(dv))){
    stop("data frame dv doesn't include all mandatory columns")
  }
  
  val_cols <- grep("62610", names(dv))
  remark_col <- grep("_cd", names(dv))
  remark_col <- remark_col[remark_col %in% val_cols]
  
  
  val_cols <- val_cols[!val_cols %in% remark_col]
  val_cols <- names(dv)[val_cols]
  remark_col <- names(dv)[remark_col]
  
  lev_dateTime <- sl_lev_va <- lev_age_cd <- ".dplyr"
  datum <- unique(gwl$sl_datum_cd)
  y_label <- sprintf("Elevation above %s, feet", datum)
  
  plot_out <- ggplot() +
    geom_line(data = dv,
              aes_string(x = "Date", y = val_cols, color = remark_col),
              linetype = "dashed") +
    geom_point(data = gwl,
               aes(x = lev_dt, y = sl_lev_va, fill = lev_age_cd),
               size = 1.5, shape = 21, color = "transparent") +
    theme_gwl() +
    labs(caption = paste("Plot created:", Sys.Date()), 
         y = y_label, x = "Years") +
    expand_limits(y = 0) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0))) +
    scale_color_manual("Daily Data",
                       values = c("A" = "blue", "P" = "red"), 
                       labels = c("A" = "Approved",
                                  "P" = "Provisional")) +
    scale_fill_manual("Water-Level Measurement",
                       values = c("A" = "blue", "P" = "red"), 
                       labels = c("A" = "Approved",
                                  "P" = "Provisional")) +
    ggtitle(title, 
            subtitle = "U.S. Geological Survey") +
    theme(legend.position = "bottom",
          legend.direction = "vertical")
  
  return(plot_out)
  
}

 

