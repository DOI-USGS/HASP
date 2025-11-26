#' Generate targets script for national aquifer data pull
#'
#' @description Opens a template of the blanks sample report. Running the function
#' will create a Rmd file.
#'
#' @param national_aquifer_cd character. 
#' @param parameter_cd character. 
#' @param statistic_cd character. 
#' @param start_date character. 
#' @param end_date character. 
#' @param file_name description
#'
#' @export
#' @examples 
#' 
#' 
#' \dontrun{
#' create_targets_script(national_aquifer_cd = "N100BSNRGB",
#'                       parameter_cd = "72019",
#'                       statistic_cd = "00003",
#'                       start_date = "1988-10-01",
#'                       end_date = "2021-01-01",
#'                       file_name = "_targets_test.R")
#' }
#' 
create_targets_script <- function(national_aquifer_cd,
                                  parameter_cd = "72019",
                                  statistic_cd = "00003",
                                  start_date,
                                  end_date,
                                  file_name = "_targets.R"){
  
  
  template_path <- system.file(package = "HASP", "templates", "aquifer_target.R")
  
  
  template_contents <- strsplit(whisker::whisker.render(xfun::read_utf8(template_path), 
                                                        data = list(national_aquifer_cd = national_aquifer_cd,
                                                                    parameter_cd = parameter_cd,
                                                                    statistic_cd = statistic_cd,
                                                                    start_date = start_date,
                                                                    end_date = end_date)), "\n")[[1]]
  
  
  new <- usethis::write_over(file_name, template_contents)
  
  if (new) {
    usethis::edit_file(save_as)
  }
  
  
}

