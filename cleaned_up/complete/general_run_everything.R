# This is a fast way to run everything.
#
# Packages: ggplot2, ggrepel, sp
# Functions stored in: general_generate_data_files.R, general_make_tree_maps.R,
# corner_points_coordinates_general.R, corner_point_names.R, find_tree_coordinates.R.
# general_generate_data_files and general_make_tree_maps call everything they need from the others.
#
# Input and output files go in a folder called (right now) "mapping_data_general".
if (!dir.exists("mapping_data_general")) dir.create("mapping_data_general", showWarnings=TRUE, recursive = FALSE)

source("general_generate_data_files.R")
source("general_make_tree_maps.R")

# This will run with any site and any data file of the same format as mapping_temp.
# general_map_setup needs the name of the data file and the site. 
# It will put data files in mapping_data_general/[sitename]/mapping_data/.
# make_tree_maps_general needs the name of the site.
# It will put plots and data files of the tree coordinates in 
# mapping_data_general/[sitename]/mapping_output/.


general_map_setup("mapping_temp.csv", "nblas")
make_tree_maps_general("nblas")
