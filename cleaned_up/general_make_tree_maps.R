# general_make_tree_maps uses find_tree_coordinates.R to find the coordinates of all the trees
# in all the cells at a site. It makes plots with the trees and the corners of each grid cell 
# and saves them to mapping_data_generalsite/mapping_output/. It also saves .csv files of the
# coordinates for all the trees in each grid cell, also to mapping_output/. 
#
# These "tree_coordinates" .csv files include an "Issue" column that tells why a tree
# has NAs instead of coordinates, or if there is a known or likely error in the tree's
# placement.
#
# What the issues mean:
# "Map_info_missing": One of the mapping data fields (distance to a known point, or 
# coordinates of a known point) is blank. Generally, if one is they all are. 
# "Distances_too_small": The sum of the distances from the tree to the left and right
# corner points is less than the length of the straight line connecting the two corners.
# "Corner_missing": One of the four grid cell corners is missing GPS coordinates.
# "Outside_cell": It was possible to plot the tree, but it landed outside its grid cell.
# "Small_RtoL": The distance between the right and left known points is small - for now,
# less than 9m - meaning small differences in the measured distance from a tree to the
# known points will translate into large differences in where the tree gets plotted. 
#


make_tree_maps_general <- function(site) {
  
  # Source find_tree_coordinates to run the calculations
  source("find_tree_coordinates.R")
  
  folderpath <- paste("mapping_data_general/", site, "/", sep="")
  
  # Get a list of the cell names, and count the cells. 
  cellnamespath <- paste(folderpath, "cells_list.csv", sep="")
  cellnames <- read.csv(cellnamespath, header=TRUE)
  ncells <- length(cellnames$cells_at_this_site)
  
  # For every grid cell...
  completed <- 1
  while (completed <= ncells){
    
    # Get the names of the files containing mapping data and corners data.
    mapdatastring <- paste(folderpath,"mapping_data/", as.character(cellnames$cells_at_this_site[completed]),"_trees.csv", sep="")
    cornersdatastring <- paste(folderpath,"mapping_data/", as.character(cellnames$cells_at_this_site[completed]), "_corners.csv", sep="")
    
    # Load mapping data and select the columns for mapping. 
    # Load corners data.
    mapdata <- read.csv(mapdatastring, header=TRUE)
    columns_for_mapping <- c("LKPx", "LKPy", "RKPx", "RKPy", "leftdist", "rightdist", "tag")
    mapdata_for_mapping <- mapdata[columns_for_mapping]
    cornerdata <- read.csv(cornersdatastring, header=TRUE)
    
    # How many trees? (= how many times to run function)
    repeats <- nrow(mapdata)
    
    # Create empty matrix to store results. 
    dataoutrows <- c(1:repeats)
    # dataoutcols <- c("UnknownX", "UnknownY", "Issue", "Tag")
    dataout_unk_x <- vector(mode = "numeric", length = repeats)
    dataout_unk_y <- vector(mode="numeric", length = repeats)
    dataout_tag <- vector(mode="integer", length = repeats)
    dataout_issue <- vector(mode="character", length=repeats)
    # dataout <- data.frame(data = NA, nrow = repeats, ncol = 4, byrow = TRUE, dimnames = list(dataoutrows, dataoutcols))
    
    # Run mapping and store results for every tree
    sofar <- 1
    while (sofar <= repeats) {
      
      # If the mapping data fields are missing, skip calculations and return
      # NA for coordinates. Add NAs and the tag number to the output matrix.
      if (is.na(mapdata_for_mapping[sofar,1]) || is.na(mapdata_for_mapping[sofar,3])) {
        dataout_unk_x[sofar] <- NA
        dataout_unk_y[sofar] <- NA
        dataout_tag[sofar] <- mapdata_for_mapping[sofar,7]
        dataout_issue[sofar] <- "Map_info_missing"
      } else {
        # If no fields are missing, run calculations.
        # Create a vector of the mapping data values for this tree.
        # Run this vector through find_tree_coordinates to get the coordinates of this tree.
        # Add the coordinates and the tag number to the grid cell's output matrix.
        datatouse <- as.numeric(mapdata_for_mapping[sofar,])
        iterationresult <- find_tree_coordinates(cornerdata, datatouse)
        
        dataout_unk_x[sofar] <- as.numeric(iterationresult[1])
        dataout_unk_y[sofar] <- as.numeric(iterationresult[2])
        dataout_tag[sofar] <- mapdata_for_mapping[sofar,7]
        dataout_issue[sofar] <- as.character(iterationresult[3])
      }
      sofar = sofar + 1
    }
    
    # Make a plot of the mapped trees and the grid cell corner points.
    # Save a .pdf of the plot to mapping_output/.
    
    outputfilepath <- paste("mapping_data_general/", site, "/mapping_output",sep="")
    if (!dir.exists(outputfilepath)) dir.create(outputfilepath, showWarnings=TRUE, recursive = FALSE)
    
    
    library(ggplot2)
    library(ggrepel)
    
    plottitle <- plottitle <- as.character(cornerdata[1,4])
    forplotbind <- cbind(dataout_unk_x, dataout_unk_y, dataout_tag)
    forplot <-as.data.frame(forplotbind)
    
    grid_cell_map <- ggplot(forplot) + geom_point(aes(x=dataout_unk_x, y=dataout_unk_y)) + 
      geom_text_repel(aes(x=dataout_unk_x, y=dataout_unk_y, label=dataout_tag)) +
      theme_bw()+
      geom_point(data=cornerdata, aes(cornerx, cornery, label=cornerpt), size=3, color="blue") +
      geom_text_repel(data=cornerdata, aes(cornerx, cornery, label=cornerpt), size=6, color="blue") +
      xlab("x-coordinate") +
      ylab("y-coordinate") +
      ggtitle(plottitle)
    plotfilename <- paste(plottitle, ".pdf", sep="")
    outputpath <- paste(folderpath, "mapping_output/", sep="")
    ggsave(plotfilename, plot=grid_cell_map, device = "pdf", path=outputpath, limitsize = TRUE)
    
    # Save the tree coordinates to subdirectory "mapping_output/.
    forfilebind <- cbind(dataout_unk_x, dataout_unk_y, dataout_issue, dataout_tag)
    forfile <- as.data.frame(forfilebind)
    filetitle <- paste(outputpath,plottitle, "_tree_coordinates.csv", sep="")
    write.csv(forfile, file=filetitle, quote=FALSE, row.names=FALSE)
    
    completed = completed + 1
  }
}

