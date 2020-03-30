#' Specific conductance and chloride
#'
#' Function creates the individual chloride versus specfic conductance plots and tables for each site
#' as well as the chloride versus specific conductance plot for all sites.
#'
#' @param x data frame returned from dataRetrieval::readNWISqw,
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
#' site_data <- L2701_example_data$QW
#' title <- paste(attr(site_data, "siteInfo")[["station_nm"]], ": Specific Conductance vs Chloride")
#' Sc_Cl_plot(site_data, title = title)
Sc_Cl_plot <- function(x, title){
  
  chloride <- sp <- ..eq.label.. <- ..rr.label.. <- ".dplyr"
  
  # Specify the plot titles using the function getParmCodeDef
  
  Cltitle <- gsub(", unfiltered", "",readNWISpCode("99220")[["parameter_nm"]])
  Cltitle <- gsub(", water", "", Cltitle)
  Sctitle <- gsub(", unfiltered", "",readNWISpCode("90095")[["parameter_nm"]])
  Sctitle <- gsub(", water", "", Sctitle)
  Sctitle <- gsub(", laboratory", "", Sctitle)
  
  Plotdata <- Sc_Cl_table(x)
  
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
#' site <- "263819081585801"
#' parameterCd <- c("00095","90095","00940","99220")
#' site_data <- dataRetrieval::readNWISqw(site, 
#'                                        parameterCd)
#' sc_cl <- Sc_Cl_table(site_data)
Sc_Cl_table <- function(x){
  
  mean_no_na <- function(x){
    mean(x, na.rm = TRUE)
  }
  
  if(!all(c("sample_dt", "parm_cd", "result_va") %in% names(x))){
    stop("data frame x doesn't include all mandatory columns")
  }
  
  sample_dt <- startDateTime <- site_no <- parm_cd <- remark_cd <- result_va <- `90095` <- `99220` <- ".dplyr"
  chloride <- sp <- ".dplyr"
  
  Plotdata <- x %>% 
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

