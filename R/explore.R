#' Explore data in the Shiny Application
#' 
#' Open an interactive app in a browser. See the "Shiny App" vignette:
#' \href{../doc/shinyApp.html}{\code{vignette("shinyApp", package = "toxEval")}} for more details. Using this 
#' function is a quick and convenient way
#' to explore data. For more customization, the R-code to 
#' produce each graph and table is displayed in the app. That is 
#' a good starting-point for a custom analysis.
#' 
#' @param browse Logical. Use browser for running Shiny app.
#' @export
#' @importFrom tools file_ext
#' @importFrom shinyAce updateAceEditor
#' @importFrom shinyAce aceEditor
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinydashboard dashboardSidebar
#' @importFrom shinydashboard sidebarMenu
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboard dashboardPage
#' @import ggplot2
#' @examples 
#' \donttest{
#' explore_aquifers()
#' }
explore_aquifers <- function(browse=TRUE){
  shiny::runApp(system.file('shiny', package='HASP'), launch.browser = browse)
}