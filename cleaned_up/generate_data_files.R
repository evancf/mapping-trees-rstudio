# generate_data_files contains three functions to make input files for the mapping 
# functions using mapping_temp.csv.
#
# Right now, you must manually tell it which site you want to map. It's set to 
# nblas. It could be generalized to every site.
#
# Functions:
# generate_corners_files() takes no arguments and writes .csv files containing the
# x,y coordinates of the corner points of every grid cell. It stores them in a 
# folder called mapping_data/nblas. 
#
# generate_trees_files() takes no arguments and writes .csv files containing the 
# tag number, species, distance to left and right known points, and coordinates of 
# left and right known points for every tree in each grid cell. These files are also
# stored in mapping_data/nblas. 
#
# generate_cells_list() takes no arguments and writes a .csv file containing a list 
# of the cells listed for a site. This might not be necessary - I think every site
# should have the same list of cells? The .csv file does NOT go in mapping_data/nblas -
# it stays in the main working directory.
#

# import whole mapping database
mapping_entire <- read.csv("mapping_temp.csv", header=TRUE)
siteslist <- unique(mapping_entire$site)
sitescount <- length(siteslist)

# choose a site and subset the main database
selected_site <- "nblas"

selected_site_entire <- mapping_entire[mapping_entire$site==selected_site,]

# select desired columns
site_col_subset <- c("island", "site", "quaddiag", "species", "leftdist", "rightdist", "tag", "RKPx", "RKPy", "LKPx", "LKPy" )
site_subsetted <- selected_site_entire[site_col_subset]

# count and list all the cells with data at this site
available_cells <- unique(site_subsetted$quaddiag)
ncells <- length(available_cells)


### function to make corners gps .csv files
### uses functions in get_corner_coordinates.R 
### writes .csv files for the corner points of every grid cell
### for now, stores them in a subdirectory "map_data_files/"
source("get_corner_coordinates.R")
generate_corners_files <- function() {
  completed <- 1
  while (completed <= ncells) {
    cell_identifier <- as.character(available_cells[completed])
    corners_coords <- cornersgpsfile(cell_identifier)
    filename <- paste("mapping_data/",selected_site,"/",cell_identifier, "_corners.csv", sep="")
    write.csv(corners_coords, file=filename, quote=FALSE, row.names=FALSE)
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
    csv_title <- paste("mapping_data/", selected_site, "/",cell_identifier, "_trees.csv",sep="")
    temp_subset <- site_subsetted[site_subsetted$quaddiag==cell_identifier, ]
    write.csv(temp_subset, file = csv_title,  quote=FALSE, row.names=FALSE)
    completed <- completed + 1
  }
}

# [5] function to make a list of the grid cells at a site
### this needs to be changed to be general
generate_cells_list <- function () {
  cells_list_title <- paste("mapping_data/", selected_site, "_cells_list.csv", sep="")
  write.csv(as.data.frame(available_cells), file=cells_list_title, quote = FALSE, row.names=FALSE)
}