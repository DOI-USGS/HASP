#' Specific conductance and chloride
#'
#' Function creates the individual chloride versus specfic conductance plots and tables for each site
#' as well as the chloride versus specific conductance plot for all sites.
#'
#' @param qw_data data frame returned from dataRetrieval::readNWISqw,
#' must include columns sample_dt, parm_cd, result_va
#' @param title character
#' @rdname sc_cl
#' @export
#' @import ggplot2
#' @importFrom scales comma
#' @importFrom ggpmisc stat_poly_eq
#' @importFrom dataRetrieval readNWISpCode
#' @examples 
#' 
#' # site <- "263819081585801"
#' # parameterCd <- c("00095","90095","00940","99220")
#' # site_data <- dataRetrieval::readNWISqw(site, 
#' #                                        parameterCd)
#' # Using package example data:
#' qw_data <- L2701_example_data$QW
#' title <- paste(attr(qw_data, "siteInfo")[["station_nm"]], ": Specific Conductance vs Chloride")
#' Sc_Cl_plot(qw_data, title = title)
Sc_Cl_plot <- function(qw_data, title){
  
  chloride <- sp <- ..eq.label.. <- ..rr.label.. <- ".dplyr"
  
  # Specify the plot titles using the function getParmCodeDef
  
  Cltitle <- gsub(", unfiltered", "",readNWISpCode("99220")[["parameter_nm"]])
  Cltitle <- gsub(", water", "", Cltitle)
  Sctitle <- gsub(", unfiltered", "",readNWISpCode("90095")[["parameter_nm"]])
  Sctitle <- gsub(", water", "", Sctitle)
  Sctitle <- gsub(", laboratory", "", Sctitle)
  
  Plotdata <- Sc_Cl_table(qw_data)
  
  if(nrow(Plotdata) == 0){
    stop("No data to make plot")
  }

  plot_out <- ggplot(data = Plotdata,
                     aes(x = sp, y = chloride)) +
    geom_point(color = "blue") +
    stat_smooth(method = "lm", color = "black", 
                formula = y ~ x , se = FALSE) +
    theme_gwl() +
    stat_poly_eq(formula = y ~ x,
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) +
    scale_y_continuous(Cltitle, 
                       labels = scales::comma) +
    scale_x_continuous(Sctitle, 
                       labels = scales::comma) +
    labs(caption = paste("Plot created:", Sys.Date())) +
    ggtitle(title, subtitle = "U.S. Geological Survey") 
  
  return(plot_out)
  
  
}

#' @rdname sc_cl
#' @export
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @examples 
#'
#' sc_cl <- Sc_Cl_table(qw_data)
Sc_Cl_table <- function(qw_data){
  
  mean_no_na <- function(x){
    mean(x, na.rm = TRUE)
  }
  
  if(!all(c("sample_dt", "parm_cd", "result_va") %in% names(qw_data))){
    stop("data frame qw_data doesn't include all mandatory columns")
  }
  
  sample_dt <- startDateTime <- site_no <- parm_cd <- remark_cd <- result_va <- `90095` <- `99220` <- ".dplyr"
  chloride <- sp <- ".dplyr"
  
  Plotdata <- qw_data %>% 
    select(Date = sample_dt, 
           parm_cd, 
           result_va) %>% 
    mutate(parm_cd = ifelse(parm_cd == "00940", "99220", parm_cd),
           parm_cd = ifelse(parm_cd == "00095", "90095", parm_cd)) %>% 
    pivot_wider(names_from = parm_cd, 
                values_from = result_va,
                values_fn = list(result_va = mean_no_na)) %>%  
    rename(chloride = `99220`,
           sp = `90095`) %>% 
    filter(!is.na(sp),
           !is.na(chloride))
  
  return(Plotdata)
  
}

#' @rdname sc_cl
#' @param pcode character pcode to plot
#' @export
#' @examples
#' title <- attr(qw_data, "siteInfo")[["station_nm"]]
#' qw_plot(qw_data, title, pcode = c("00095", "90095"))
#' qw_plot(qw_data, title, pcode = c("00940","99220"))
qw_plot <- function(qw_data, title,
                    pcode = c("00095", "90095")){
  
  if(!all(c("sample_dt", "result_va", "remark_cd", "parm_cd") %in% names(qw_data))){
    stop("data frame gwl doesn't include all mandatory columns")
  }
  
  sample_dt <- result_va <- remark_cd <- parm_cd <- ".dplyr"
  
  qw_data <- qw_data %>% 
    filter(parm_cd %in% pcode)
  
  y_label <- gsub(", unfiltered", "",readNWISpCode(pcode[1])[["parameter_nm"]])
  y_label <- gsub(", water", "", y_label)

  plot_out <- ggplot() +
    geom_point(data = qw_data,
               aes(x = sample_dt, y = result_va),
               size = 1.5, color = "blue") +
    theme_gwl() +
    labs(caption = paste("Plot created:", Sys.Date()), 
         y = y_label, x = "Date") +
    expand_limits(y = 0) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +

    ggtitle(title, 
            subtitle = "U.S. Geological Survey") +
    theme(legend.position = "bottom",
          legend.direction = "vertical")
  
  return(plot_out)
  
}