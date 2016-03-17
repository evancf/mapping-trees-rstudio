# make_tree_maps uses find_tree_coordinates to find the coordinates of all the trees
# in a grid cell. It makes a plot with the trees and the corners of the grid cell 
# and saves it (currently to mapping_output/). It also saves a .csv file of the
# coordinates for all the trees in a grid cell, also to mapping_output/. 
#
# If there are missing fields for a tree, or the calculations don't work out, the
# tree will not be on the plot and its coordinates will be NAs in the data file.
#
# To add: a field in the data file that says why a tree is NA, or identifies trees
# that are likely to be plotted wrong (outside the grid cell, or the distance between the 
# two known points is smaller than usual and therefore likely to amplify a small
# measurement error). 
# 
# Right now this is set up to work for nblas on Guam. It can be generalized so any
# site is easy to process.
# 


make_tree_maps <- function() {
  
  # Source find_tree_coordinates to run the calculations
  source("find_tree_coordinates.R")
  
  # Get a list of the cell names, and count the cells. 
  cellnames <- read.csv("mapping_data/nblas_cells_list.csv", header=TRUE)
  ncells <- length(cellnames$available_cells)
  
  # For every grid cell...
  completed <- 1
  while (completed <= ncells){
    
    # Get the names of the files containing mapping data and corners data.
    mapdatastring <- paste("mapping_data/nblas/", as.character(cellnames$available_cells[completed]),"_trees.csv", sep="")
    cornersdatastring <- paste("mapping_data/nblas/", as.character(cellnames$available_cells[completed]), "_corners.csv", sep="")
    
    # Load mapping data and select the columns for mapping. 
    # Load corners data.
    mapdata <- read.csv(mapdatastring, header=TRUE)
    columns_for_mapping <- c("LKPx", "LKPy", "RKPx", "RKPy", "leftdist", "rightdist")
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
        dataout_tag[sofar] <- mapdata[sofar,7]
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
        dataout_tag[sofar] <- mapdata[sofar,7]
        dataout_issue[sofar] <- as.character(iterationresult[3])
      }
      sofar = sofar + 1
    }
    
    # Make a plot of the mapped trees and the grid cell corner points.
    # Save a .pdf of the plot to mapping_output/.
    
    library(ggplot2)
    library(ggrepel)
    
    plottitle <- plottitle <- as.character(cornerdata[1,4])
    forplotcols <- c("UnknownX", "UnknownY", "Tag")
    forplotbind <- cbind(dataout_unk_x, dataout_unk_y, dataout_tag)
    forplot <-as.data.frame(forplotbind)
    
    grid_cell_map <- ggplot(forplot) + geom_point(aes(dataout_unk_x, dataout_unk_y)) + 
      geom_text_repel(aes(dataout_unk_x, dataout_unk_y, label=dataout_tag)) +
      theme_bw()+
      geom_point(data=cornerdata, aes(cornerx, cornery, label=cornerpt), size=3, color="blue") +
      geom_text_repel(data=cornerdata, aes(cornerx, cornery, label=cornerpt), size=6, color="blue") +
      xlab("x-coordinate") +
      ylab("y-coordinate") +
      ggtitle(plottitle)
    plotfilename <- paste(plottitle, ".pdf", sep="")
    ggsave(plotfilename, plot=grid_cell_map, device = "pdf", path="mapping_output/", limitsize = TRUE)
    
    # Save the tree coordinates to subdirectory "mapping_output/.
    forfilebind <- cbind(dataout_unk_x, dataout_unk_y, dataout_issue, dataout_tag)
    forfile <- as.data.frame(forfilebind)
    filetitle <- paste("mapping_output/",plottitle, "_tree_coordinates.csv", sep="")
    write.csv(forfile, file=filetitle, quote=FALSE, row.names=FALSE)
    
    completed = completed + 1
  }
}

