# general_map_setup("file name", "site name") takes the name of a data file (formateed 
# like mapping_temp.csv) and the name of a site (e.g. "nblas"). It creates input files 
# to be used by the mapping functions. The input files are stored in a subdirectory
# labeled by the site name: mapping_data_general/[sitename]/mapping_data. 
# It can handle sites one at a time (> general_map_setup("mapping_temp.csv", "nblas"))
# All the sites at once (> general_map_setup("mapping_temp.csv", "all")) still has bugs.

general_map_setup <- function(master_file_name, chosen_site) {
  # Import whole mapping database
  mapping_entire <- read.csv(master_file_name, header=TRUE)
  
  
  # Identify a site and subset the main database
  if (!identical(chosen_site, "all")) {
    mapping_selected <- mapping_entire[mapping_entire$site==chosen_site,]
  } else {
    mapping_selected <- mapping_entire
  }
  
  # Count how many sites you're working with. 
  # This is supposed to be a loop to run many sites from one command, but something
  # doesn't quite work about it. One site at a time works great. 
  siteslist <- unique(mapping_selected$site)
  sitescount <- length(siteslist)
  
  # Make directories for each site, if they don't exist already.
  dirsmade <- 1
  while (dirsmade <= sitescount) {
    current_site <- as.character(siteslist[dirsmade])
    datapath <- paste("mapping_data_general/", current_site,sep="")
    if (!dir.exists(datapath)) dir.create(datapath, showWarnings=TRUE, recursive = FALSE)
    dirsmade <- dirsmade + 1 
  }
  
  # Subset the main data file to get all the data for each site, and save that file 
  # in the site's directory. 
  mainfilesmade <- 1
  while (mainfilesmade <= sitescount) {
    current_site <- as.character(siteslist[mainfilesmade])
    filepath <- paste("mapping_data_general/", current_site, "/mapping-complete.csv", sep="")
    data_temp <- mapping_selected[mapping_selected$site==current_site,]
    write.csv(data_temp, file=filepath, quote=FALSE, row.names=FALSE)
    mainfilesmade <- mainfilesmade + 1
  }
  
  # Find the gps coordinates of all the grid points at a site.
  # Search "lefttarget", "righttarget", "LKPx", "LKPy", "RKPx", and "RKPy" 
  # to find a match for each grid point name. Search the left first and use 
  # the first match found.
  # Points that do not have coordinates in LKPx/y or RKPx/y get NAs. 
  # This is common for the z-e area. 
  # Save this "gps library" in the site's directory.
  gpsfilesmade <- 1
  cornernameslist <- read.csv("point_names.csv")
  ncorners <- nrow(cornernameslist)
  
  while (gpsfilesmade <= sitescount) {
    current_site <- as.character(siteslist[gpsfilesmade])
    all_data_path <- paste("mapping_data_general/", current_site, "/mapping-complete.csv", sep="")
    all_data <- read.csv(all_data_path, header=TRUE)
    gps_data_path <- paste("mapping_data_general/", current_site, "/gps_library.csv", sep="")
    cols_for_gps <- c("site", "lefttarget", "righttarget", "LKPx", "LKPy", "RKPx", "RKPy")
    data_for_gps <- all_data[cols_for_gps]
    data_for_gps <- data_for_gps[complete.cases(data_for_gps),]
    rows <- nrow(data_for_gps)
    
    corner_x <- vector(mode="numeric", length = ncorners)
    corner_y <- vector(mode="numeric", length = ncorners)
    
    xyfound <- 1
    while (xyfound <= ncorners) {
      targetname <- as.character(cornernameslist[xyfound, 1])
      checked <- 1
      while (checked <= rows) {
        if (identical(as.character(data_for_gps[checked, 2]), targetname)) {
          corner_x[xyfound] <- as.numeric(data_for_gps[checked, 4])
          corner_y[xyfound] <- as.numeric(data_for_gps[checked, 5])
          checked <- rows + 1
        }
        checked <- checked + 1
      }
      
      if (corner_x[xyfound] < 1) {
        checked <- 1
        while (checked <= rows) {
          if (identical(as.character(data_for_gps[checked, 3]), targetname)) {
            corner_x[xyfound] <- as.numeric(data_for_gps[checked, 6])
            corner_y[xyfound] <- as.numeric(data_for_gps[checked, 7])
            
            checked <- rows + 1
          }
          checked <- checked + 1
        }
      }
      
      if (corner_x[xyfound] <1) {
        corner_x[xyfound] <- NA
        corner_y[xyfound] <- NA
      }
      
      xyfound <- xyfound + 1
    }
    
    gpstable <- cbind(cornernameslist, corner_x, corner_y)
    gpsdf <- as.data.frame(gpstable)
    
    write.csv(gpsdf, file = gps_data_path, quote=FALSE, row.names=FALSE)
    
    gpsfilesmade <- gpsfilesmade + 1
  }
  
  # Find the names and coordinates of the four corners of each grid cell.
  # Save files containing the cell name, corners, and coordinates for the corners
  # of every grid cell to /mapping_data/. 
  # This section uses functions in corner_point_names.R and 
  # corner_point_coordinates_general.R.
  
  source("corner_point_names.R")
  source("corner_point_coordinates_general.R")
  
  cornerfilesmade <- 1
  while (cornerfilesmade <= sitescount) {
    current_site <- as.character(siteslist[cornerfilesmade])
    
    # Make a folder to hold mapping data files
    cornerdatapath <- paste("mapping_data_general/", current_site, "/mapping_data",sep="")
    if (!dir.exists(cornerdatapath)) dir.create(cornerdatapath, showWarnings=TRUE, recursive = FALSE)
    
    # Make a list of the cells at this site. Find out how many cells there are to run.
    cells_at_this_site <- unique(mapping_selected$quaddiag)
    cells_at_this_site <- as.data.frame(cells_at_this_site)
    ncells <- nrow(cells_at_this_site)
    
    # Save the list so other functions can use it later. 
    cell_list_file_path <- paste("mapping_data_general/", current_site, "/cells_list.csv", sep="")
    write.csv(cells_at_this_site, file=cell_list_file_path, quote=FALSE, row.names=FALSE)
    
    # Find the names and coordinates of the four corners of each cell,
    # based on the quaddiag value. 
    # Save corners coordinates files to mapping_data. 
    completed <- 1
    while (completed <= ncells) {
      cell_identifier <- as.character(cells_at_this_site[completed, 1])
      corners_names <- corner_point_names(cell_identifier)
      corners_coordinates <- data.frame(corner_point_coordinates_general(corners_names, current_site))
      filename <- paste("mapping_data_general/",current_site, "/mapping_data/",cell_identifier, "_corners.csv", sep="")
      write.csv(corners_coordinates, file=filename, quote=FALSE, row.names=FALSE)
      completed <- completed + 1
    }
    cornerfilesmade <- cornerfilesmade + 1
  }
  
  # Subset the columns in mapping_temp (and mapping-complete.csv) to make files 
  # for the mapping functions to use. 
  # Save these files to mapping_data.
  col_subset <- c("island", "site", "quaddiag", "species", "leftdist", "rightdist", "tag", "RKPx", "RKPy", "LKPx", "LKPy" )
  mapping_sub <- mapping_selected[col_subset]
  
  completed <- 1
  while (completed <= ncells) {
    cell_identifier <- as.character(cells_at_this_site[completed, 1])
    csv_title <- paste("mapping_data_general/", current_site, "/mapping_data/",cell_identifier, "_trees.csv",sep="")
    temp_subset <- mapping_sub[mapping_sub$quaddiag==cell_identifier, ]
    write.csv(temp_subset, file = csv_title,  quote=FALSE, row.names=FALSE)
    completed <- completed + 1
  }
}