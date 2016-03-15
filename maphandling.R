## Code to take a quaddiag value, find it on a grid, and produce the 4 corner points of the cell.

getcorners <- function(word) {
  quaddiag <- as.character(word)
  
  corner_names_out <- matrix(data=NA, nrow=2, ncol=3, byrow=TRUE, dimnames=NULL)
  corner_names_out[1,3] <- quaddiag
  corner_names_out[2,3] <- quaddiag
  
  letterindexone <- 0
  numberindexone <- 1
  
  if (grepl("z", quaddiag) ) {
    if(grepl("*", quaddiag)) {
      interiorcorner <- substring(quaddiag, 6,7)
      if (identical(interiorcorner, "a0")) {
        letterindexone <- 1
        numberindexone <- 1
      }
      if (identical(interiorcorner, "e0")) {
        letterindexone <- 6
        numberindexone <- 1
      }
      if (identical(interiorcorner, "a4")) {
        letterindexone <- 1
        numberindexone <- 6
      }
      if (identical(interiorcorner, "e4")) {
        letterindexone <- 6
        numberindexone <- 6
      }
    } 
    
    diagnostic <- substring(quaddiag, 5,5)
    
    if (identical(diagnostic, "a")) {
      letterindexone <- 1
      numberchar <- substring(quaddiag, 6,6)
      numberint <- as.integer(numberchar)
      numberindexone <- numberint + 1
    } 
    
    if (identical(diagnostic, "e")) {
      letterindexone <- 6
      numberchar <- substring(quaddiag, 6,6)
      numberint <- as.integer(numberchar)
      numberindexone <- numberint + 1
    }
    
    if (identical(diagnostic, "4")) {
      letter <- substring(quaddiag, 6,6)
      if (identical(letter, "a")) letterindexone <- 2
      if (identical(letter, "b")) letterindexone <- 3
      if (identical(letter, "c")) letterindexone <- 4
      if (identical(letter, "d")) letterindexone <- 5
      if (identical(letter, "e")) letterindexone <- 6
      numberindexone <- 6
      
    }
    
    if (identical(diagnostic, "0")) {
      letter <- substring(quaddiag, 6,6)
      if (identical(letter, "a")) letterindexone <- 2
      if (identical(letter, "b")) letterindexone <- 3
      if (identical(letter, "c")) letterindexone <- 4
      if (identical(letter, "d")) letterindexone <- 5
      if (identical(letter, "e")) letterindexone <- 6
      numberindexone <- 1
      
    }
  } else {
    
    cornerone <- substring(quaddiag,1,2)
    cornerone
    cornertwo <- substring(quaddiag, 4)
    cornertwo
    
    corneroneletter <- substring(cornerone,1,1)
    corneronenumber <- substring(cornerone,2,2)
    
    corneroneletter
    corneronenumber
    corneronenumberint <- as.integer(corneronenumber)
    numberindexone <- corneronenumberint + 2
    
    if(identical(corneroneletter, "a")) letterindexone <- 2
    if(identical(corneroneletter, "b")) letterindexone <- 3
    if(identical(corneroneletter, "c")) letterindexone <- 4
    if(identical(corneroneletter, "d")) letterindexone <- 5
    if(identical(corneroneletter, "e")) letterindexone <- 6
  }
  
  letterindextwo <- letterindexone + 1
  letterindexthree <- letterindextwo
  letterindexfour <- letterindexone
  
  numberindextwo <- numberindexone
  numberindexthree <- numberindexone + 1
  numberindexfour <- numberindexthree
  
  corner_names_source <- read.csv("nblas_gps_names.csv", header=FALSE)
  
  corner_names_source_matrix <- as.matrix(corner_names_source)
  
  
  corner_names_out[1,1] <- (corner_names_source_matrix[numberindexone, letterindexone])
  corner_names_out[1,2] <- (corner_names_source_matrix[numberindextwo, letterindextwo])
  corner_names_out[2,2] <- (corner_names_source_matrix[numberindexthree, letterindexthree])
  corner_names_out[2,1] <- (corner_names_source_matrix[numberindexfour, letterindexfour])

  
  return(corner_names_out)
}

