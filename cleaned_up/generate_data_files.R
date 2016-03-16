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


### generate_corners_files is a function to write .csv files of the coordinates of the
### four corners of a grid cell and store them in a subdirectory "map_data_files/[site]/"

generate_corners_files <- function() {
  # Source a function to get the 4 corner point names from the quaddiag value, and
  # a function to get the coordinates for each of the 4 points. 
  source("corner_point_names.R")
  source("corner_point_coordinates.R")
  
  # For every grid cell at a site, get the names and coordinates of the corner points.
  # Write the coordinates to a .csv file. Name it according to the grid cell, and put
  # it in a folder according to the site. 
  completed <- 1
  while (completed <= ncells) {
    cell_identifier <- as.character(available_cells[completed])
    corners_names <- corner_point_names(cell_identifier)
    corners_coordinates <- data.frame(corner_point_coordinates(corners_names))
    filename <- paste("mapping_data/",selected_site,"/",cell_identifier, "_corners.csv", sep="")
    write.csv(corners_coordinates, file=filename, quote=FALSE, row.names=FALSE)
    completed <- completed + 1
  }
}


### generate_trees_files is a function to write .csv files to be used as input for the
### mapping function (make_tree_maps, which calls find_tree_coordinates). It stores 
### them in "/map_data_files/[site]". 

generate_trees_files <- function() {
  # For every grid cell at a site, subset the site-specific data frame by quaddiag.
  # Write this data frame to a .csv file labeled by grid cell, in a folder labeled
  # by site. 
  completed <- 1
  while (completed <= ncells) {
    cell_identifier <- as.character(available_cells[completed])
    csv_title <- paste("mapping_data/", selected_site, "/",cell_identifier, "_trees.csv",sep="")
    temp_subset <- site_subsetted[site_subsetted$quaddiag==cell_identifier, ]
    write.csv(temp_subset, file = csv_title,  quote=FALSE, row.names=FALSE)
    completed <- completed + 1
  }
}


### generate_cells_list is a function to make a list of the grid cells at a site.
### The list is stored in mapping_data/.
### It might not be necessary to generate a list for every site - shouldn't they all
### be the same?

generate_cells_list <- function () {
  cells_list_title <- paste("mapping_data/", selected_site, "_cells_list.csv", sep="")
  write.csv(as.data.frame(available_cells), file=cells_list_title, quote = FALSE, row.names=FALSE)
}