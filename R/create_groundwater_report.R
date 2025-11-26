#' Generate HASP report
#'
#' @description Opens a template of the blanks sample report. Running the function
#' will create a Rmd file. The file can be "knit" as-is, or adjusted before rendering.
#'
#' @param siteID character. USGS site ID, should be a groundwater site.
#' @param report_folder character. Report folder, can be full path or partial.
#' @param report_name character. Base name of report.
#' @param output_type should be either "word", "html", or "pdf".
#'
#' @export
#' @examples 
#' 
#' 
#' \dontrun{
#' create_groundwater_report("USGS-253029080295601",
#'                           report_name = "example_report",
#'                           report_folder = "reports",
#'                           output_type = "word")
#'                           
#' create_groundwater_report("USGS-253029080295601",
#'                           report_name = "example_report",
#'                           report_folder = "reports",
#'                           output_type = "html")
#' }
#' 
create_groundwater_report <- function(siteID,
                                      report_name,
                                      report_folder,
                                      output_type = "word"){
  
  checkmate::assert_string(siteID)
  output_type <- match.arg(output_type, c("word", "html", "pdf"),
                           several.ok = FALSE)

  report_path <- file.path(report_folder)
  dir.create(report_folder, showWarnings = FALSE)

  
  template_path <- system.file(package = "HASP", "templates", "HASP_report.Rmd")
  
  
  template_contents <- strsplit(whisker::whisker.render(xfun::read_utf8(template_path), 
                                                        data = list(site = siteID,
                                                                    output_type = output_type)), "\n")[[1]]
  
  
  report_name <- gsub(" ", "_", report_name)
  save_as <- file.path(report_path, paste0(report_name, ".Rmd"))
  new <- usethis::write_over(save_as, template_contents)

  if (new) {
    usethis::edit_file(save_as)
  }
  
}

