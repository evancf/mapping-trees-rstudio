# import a data file
# subset it by site (or don't if args[1] = all)
# make a directory for each site
# put a master data file in each site directory
# get the gps coordinates of the corners of each site
# put a gps library in each site directory

# for now
# master_file_name <- "mapping_temp.csv"
# chosen_site <- "ritd"

general_map_setup <- function(master_file_name, chosen_site) {
  # import whole mapping database
  mapping_entire <- read.csv(master_file_name, header=TRUE)
  

  # choose a site and subset the main database
  if (!identical(chosen_site, "all")) {
    mapping_selected <- mapping_entire[mapping_entire$site==chosen_site,]
  } else {
    mapping_selected <- mapping_entire
  }
  
  # how many sites? this should work for 1 or many
  siteslist <- unique(mapping_selected$site)
  sitescount <- length(siteslist)
  
  # make directories if they don't exist already
  dirsmade <- 1
  while (dirsmade <= sitescount) {
    current_site <- as.character(siteslist[dirsmade])
    datapath <- paste("~/Desktop/mapping-trees-copy/cleaned_up/mapping_data_general/", current_site,sep="")
    if (!dir.exists(datapath)) dir.create(datapath, showWarnings=TRUE, recursive = FALSE)
    dirsmade <- dirsmade + 1 
  }
  
  # make and save master data file(s) to the directories
  mainfilesmade <- 1
  while (mainfilesmade <= sitescount) {
    current_site <- as.character(siteslist[mainfilesmade])
    filepath <- paste("~/Desktop/mapping-trees-copy/cleaned_up/mapping_data_general/", current_site, "/mapping-complete.csv", sep="")
    data_temp <- mapping_selected[mapping_selected$site==current_site,]
    write.csv(data_temp, file=filepath, quote=FALSE, row.names=FALSE)
    mainfilesmade <- mainfilesmade + 1
  }
  
  # make gps libraries
  gpsfilesmade <- 1
  cornernameslist <- read.csv("~/Desktop/mapping-trees-copy/cleaned_up/point_names.csv")
  ncorners <- nrow(cornernameslist)
  
  while (gpsfilesmade <= sitescount) {
    current_site <- as.character(siteslist[gpsfilesmade])
    all_data_path <- paste("~/Desktop/mapping-trees-copy/cleaned_up/mapping_data_general/", current_site, "/mapping-complete.csv", sep="")
    all_data <- read.csv(all_data_path, header=TRUE)
    gps_data_path <- paste("~/Desktop/mapping-trees-copy/cleaned_up/mapping_data_general/", current_site, "/gps_library.csv", sep="")
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
  
  # make corner files?
  source("corner_point_names.R")
  source("corner_point_coordinates_general.R")
  
  cornerfilesmade <- 1
  while (cornerfilesmade <= sitescount) {
    current_site <- as.character(siteslist[cornerfilesmade])
    
    cornerdatapath <- paste("mapping_data_general/", current_site, "/mapping_data",sep="")
    if (!dir.exists(cornerdatapath)) dir.create(cornerdatapath, showWarnings=TRUE, recursive = FALSE)
    
    
  # For every grid cell at a site, get the names and coordinates of the corner points.
  # Write the coordinates to a .csv file. Name it according to the grid cell, and put
  # it in a folder according to the site. 
  cells_at_this_site <- unique(mapping_selected$quaddiag)
  cells_at_this_site <- as.data.frame(cells_at_this_site)
  ncells <- nrow(cells_at_this_site)
  
  # also make a .csv of the cell name list
  cell_list_file_path <- paste("mapping_data_general/", current_site, "/cells_list.csv", sep="")
  write.csv(cells_at_this_site, file=cell_list_file_path, quote=FALSE, row.names=FALSE)
  
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
  
  # make tree mapping data files 
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