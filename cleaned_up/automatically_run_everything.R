# automatically_run_everything is a fast way to run all the code. Right now,
# it focuses on nblas on Guam.
# These commands should work just as they are.
# First you make the data files you'll need, working from mapping_temp.csv
source("generate_data_files.R")
generate_corners_files()
generate_trees_files()
generate_cells_list()
rm(list=ls())

# Then call make_tree_maps to calculate the locations of all the trees, plot them
# on a map, and save maps and coordinates (to the mapping_output folder). 
source("make_tree_maps.R")
make_tree_maps()
