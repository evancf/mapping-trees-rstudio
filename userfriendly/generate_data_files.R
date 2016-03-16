# contains functions...
# maybe change this so functions are stored in a folder and this just runs them all

# [2] function to make gps_library.csv
### To be written. 
### gps_library is a table of gps coordinates for the named grid points at a site.
### For nblas, you can use nblas_gps_library.csv. I got this from the LKPx/LKPy columns of mapping_temp.csv.
### gps_library will be used by the functions in get_corner_coordinates.R

#[3-4]
# import whole nblas database
nblas_entire <- read.csv("mapping_nblas_master.csv", header=TRUE)

# select desired columns
nblas_col_subset <- c("island", "site", "quaddiag", "species", "leftdist", "rightdist", "tag", "RKPx", "RKPy", "LKPx", "LKPy" )
nblas_subsetted <- nblas_entire[nblas_col_subset]

# create data frames for each cell (by quaddiag)
## count and list all the cells with data at this site
available_cells <- unique(nblas_subsetted$quaddiag)
ncells <- length(available_cells)

# [3] function to make corners gps .csv files
### uses functions in get_corner_coordinates.R 
### writes .csv files for the corner points of every grid cell
### for now, stores them in a subdirectory "map_data_files"
source("get_corner_coordinates.R")
generate_corners_files <- function() {
  completed <- 1
  while (completed <= ncells) {
    cell_identifier <- as.character(available_cells[completed])
    cornersgpsfile(cell_identifier)
    completed <- completed + 1
  }
}
# [4] function to make mapping input .csv files
### writes .csv files of desired data for each grid cell
### for now, stores them in a subdirectory "map_data_files"
generate_trees_files <- function() {
  completed <- 1
  while (completed <= ncells) {
    cell_identifier <- as.character(available_cells[completed])
    csv_title <- paste("mapping_data/", cell_identifier, "_trees.csv",sep="")
    temp_subset <- nblas_subsetted[nblas_subsetted$quaddiag==cell_identifier, ]
    write.csv(temp_subset, file = csv_title,  quote=FALSE, row.names=FALSE)
    completed <- completed + 1
  }
}

# [5] function to make a list of the grid cells at a site
### this needs to be changed to be general
generate_cells_list <- function () {
  write.csv(as.data.frame(available_cells), file="mapping_data/nblas_cell_names.csv", quote = FALSE, row.names=FALSE)
}