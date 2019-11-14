#' markerLookup
#' 
#' Looks up based on GenericMarkerLookupTable.csv
#' 
#' @export
#' @param inputTrend character?
#' 
markerLookup <- function(inputTrend){
  
  markerLookupSubset <- markerTable %>% 
    filter(Trend == inputTrend)
  
  markerDescription <- markerLookupSubset$MarkerDescription
  
  return(markerDescription)
}



# To make the sysdata.rda:
# markerTable <- data.frame(Trend = c("Insufficient data",
#                                     "Up",
#                                     "Down",
#                                     "Not significant",
#                                     "None"),
#                           MarkerDescription = c("Square",
#                                                 "UpArrow",
#                                                 "DownArrow",
#                                                 "Circle",
#                                                 "Circle"),
#                           stringsAsFactors = FALSE)
# 
# markerTable2 <- data.frame(trendType = c(rep("Five Year",5),
#                                          rep("Twenty Year",5),
#                                          rep("Overall",5)),
#                            trend = c(rep(c("Insufficient data",
#                                            "None",
#                                            "Not significant",
#                                            "Up", "Down"),2),
#                                      "Insufficient data",
#                                      "None","Up","Down","Opposite"),
#                            markerDescription = c(rep(c("Square",
#                                                        "Circle",
#                                                        "Circle",
#                                                        "UpArrow",
#                                                        "DownArrow"),2),
#                                                  "Square", "Circle",
#                                                  "UpArrow", "DownArrow",
#                                                  "DoubleHeadedArrow"),
#                            r_lwd = c(rep("",3), 2,2,rep("",3),3,3,rep("",5)), 
#                            stringsAsFactors = FALSE)
# save(markerTable, markerTable2, file = "sysdata.rda", compress = "xz")
