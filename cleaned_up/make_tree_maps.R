make_tree_maps <- function() {
# source find coordinates
source("find_tree_coordinates.R")

cellnames <- read.csv("mapping_data/nblas_cell_names.csv", header=TRUE)
ncells <- length(cellnames$available_cells)

completed <- 1

while (completed <= ncells){
  
  mapdatastring <- paste("mapping_data/", as.character(cellnames$available_cells[completed]),"_trees.csv", sep="")
  cornersdatastring <- paste("mapping_data/", as.character(cellnames$available_cells[completed]), "_corners.csv", sep="")
  
  mapdata <- read.csv(mapdatastring, header=TRUE)
  columns_for_mapping <- c("LKPx", "LKPy", "RKPx", "RKPy", "leftdist", "rightdist")
  mapdata_for_mapping <- mapdata[columns_for_mapping]
  
  cornerdata <- read.csv(cornersdatastring, header=TRUE)
  
  # How many rows? (= how many times to run function)
  repeats <- nrow(mapdata)
  
  # Create empty matrix to store results. 
  dataoutrows <- c(1:repeats)
  dataoutcols <- c("UnknownX", "UnknownY", "Tag")
  dataout <- matrix(data = NA, nrow = repeats, ncol = 3, byrow = TRUE, dimnames = list(dataoutrows, dataoutcols))
  
  # Run mapping and store results for every row
  sofar <- 1
  while (sofar <= repeats) {
    
    if (is.na(mapdata_for_mapping[sofar,1])) {
      dataout[sofar, 1:2] <- NA
      dataout[sofar, 3] <- mapdata[sofar,7]
      #    dataout[sofar, 4] <- as.character(mapdata[sofar,4])
    } else {
      datatouse <- as.numeric(mapdata_for_mapping[sofar,])
      iterationresult <- find_tree_coordinates(cornerdata, datatouse)
      
      dataout[sofar, 1:2] <- iterationresult
      dataout[sofar, 3] <- mapdata[sofar,7]
      #dataout[sofar, 4] <- as.character(mapdata[sofar,4])
    }
    sofar = sofar + 1
  }
  
  # Place mapped trees on a plot
  library(ggplot2)
  library(ggrepel)
  
  plottitle <- plottitle <- as.character(cornerdata[1,4])
  filetitle <- paste("mapping_output/",plottitle, "_tree_coordinates.csv", sep="")
  write.csv(dataout, file=filetitle, quote=FALSE, row.names=FALSE)
  forplot <- as.data.frame(dataout[1:repeats,1:3])
  
  visualmap <- ggplot(forplot) + geom_point(aes(UnknownX, UnknownY)) + 
    geom_text_repel(aes(UnknownX, UnknownY, label=Tag)) +
    theme_bw()+
    geom_point(data=cornerdata, aes(cornerx, cornery, label=cornerpt), size=3, color="blue") +
    geom_text_repel(data=cornerdata, aes(cornerx, cornery, label=cornerpt), size=6, color="blue") +
    xlab("x-coordinate") +
    ylab("y-coordinate") +
    ggtitle(plottitle)
  visualmap
  plotfilename <- paste(plottitle, ".pdf", sep="")
  ggsave(plotfilename, plot=visualmap, device = "pdf", path="mapping_output/", limitsize = TRUE)
  
  completed = completed + 1
}
}
