source("generate_data_files.R")
generate_corners_files()
generate_trees_files()
generate_cells_list()
rm(list=ls())

source("make_tree_maps.R")
make_tree_maps()
