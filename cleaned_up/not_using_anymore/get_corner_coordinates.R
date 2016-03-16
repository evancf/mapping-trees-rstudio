# get_corner_coordinates takes the quaddiag value of a grid cell and returns 
get_corner_coordinates <- function(cell_name_in) {
  source("corner_point_names.R")
  source("corner_point_coordinates.R")
  
  cell_name <- as.character(cell_name_in) # the quaddiag value
  corners_names <- corner_point_names(cell_name)
  
  corners_names
  
  corners_coordinates <- data.frame(corner_point_coordinates(corners_names))
  
  return (corners_coordinates)
  
#   filename <- paste("mapping_data/",cell_name, "_corners.csv", sep="")
#   
#   write.csv(corners_coordinates, file=filename, quote=FALSE, row.names=FALSE)
}