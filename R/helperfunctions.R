#' getParmCodeDef
#' 
#' Function provides the text description for the parameter codes
#' 
#' @param pc parameter code
#' @export
#' @examples 
#' pc <- "99220"
#' getParmCodeDef(pc)
#' #TODO: replace this function with readNWISpcode!
getParmCodeDef <- function(pc){
  
  switch(pc, 
         "99220" = "Chloride concentration, in milligrams per liter",
         "90095" = "Specific conductance, in microsiemens per liter",
         "72020" = "Water level elevation, in feet"
         )
}



