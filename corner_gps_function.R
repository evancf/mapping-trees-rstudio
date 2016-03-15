# cornernames should be a matrix

# make it a function once it works
cornergps <- function(cornernames) {
  # empty output matrix
  cornersrow <- c("1", "2", "3", "4")
  cornerscol <- c("cornerpt", "cornerx", "cornery", "cell_name")
  cornerscoordinates <- matrix(data=NA, nrow=4, ncol=4, 
                               byrow=TRUE, 
                               dimnames=list(cornersrow, cornerscol))
  cornerscoordinates[1:4,4] <- cornernames[1,3]
  
  # get names from input
  corner_one_name <- cornernames[1,1]
  corner_two_name <- cornernames[1,2]
  corner_three_name <- cornernames[2,2]
  corner_four_name <- cornernames[2,1]
  
  # match each name to its gps point
  gps_library <- read.csv("nblas_gps_library.csv")
  
  # for first corner
  attempt <- 1
  possibleattempts <- nrow(gps_library)
  while (attempt <= possibleattempts) {
    name_character <- as.character(gps_library[attempt, 1])
    if (identical(corner_one_name, name_character)) {
      cornerscoordinates[1,1] <- corner_one_name
      cornerscoordinates[1,2] <- gps_library[attempt,2]
      cornerscoordinates[1,3] <- gps_library[attempt,3]
    }
    attempt = attempt + 1
  }
  
  # for second corner
  attempt <- 1
  possibleattempts <- nrow(gps_library)
  while (attempt <= possibleattempts) {
    name_character <- as.character(gps_library[attempt, 1])
    if (identical(corner_two_name, name_character)) {
      cornerscoordinates[2,1] <- corner_two_name
      cornerscoordinates[2,2] <- gps_library[attempt,2]
      cornerscoordinates[2,3] <- gps_library[attempt,3]
    }
    attempt = attempt + 1
  }
  
  # for third corner
  attempt <- 1
  possibleattempts <- nrow(gps_library)
  while (attempt <= possibleattempts) {
    name_character <- as.character(gps_library[attempt, 1])
    if (identical(corner_three_name, name_character)) {
      cornerscoordinates[3,1] <- corner_three_name
      cornerscoordinates[3,2] <- gps_library[attempt,2]
      cornerscoordinates[3,3] <- gps_library[attempt,3]
    }
    attempt = attempt + 1
  }
  
  # for fourth corner
  attempt <- 1
  possibleattempts <- nrow(gps_library)
  while (attempt <= possibleattempts) {
    name_character <- as.character(gps_library[attempt, 1])
    if (identical(corner_four_name, name_character)) {
      cornerscoordinates[4,1] <- corner_four_name
      cornerscoordinates[4,2] <- gps_library[attempt,2]
      cornerscoordinates[4,3] <- gps_library[attempt,3]
    }
    attempt = attempt + 1
  }
    
  return(cornerscoordinates)
  
}