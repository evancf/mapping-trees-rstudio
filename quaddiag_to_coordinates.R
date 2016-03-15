# Function to make a .csv of corner names and gps coordinates given the quaddiag value...

cornersgps <- function(cell_name_in) {
source("maphandling.R")
source("corner_gps_function.R")

cell_name <- cell_name_in # the quaddiag value
corners_names <- getcorners(cell_name)

corners_names

corners_coordinates <- data.frame(cornergps(corners_names))

corners_coordinates

filename <- paste(cell_name, "corners.csv", sep="")

write.csv(corners_coordinates, file=filename, quote=FALSE, row.names=FALSE)
}