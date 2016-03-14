# x is an array of:
# 1-8: the coordinates of the corners of the grid cell as: 
#      corner one x, corner one y, corner two x, corner two y, etc.
# 9-10: the coordinates of one known point as x, y
# 11-12: the coordinates of the second known point as x, y
# 13-14: the distances from the unknown point to the first and second known points, respectively, as x,y.

# mapping(x) returns the x and y of the unknown point.

# test data: use site = NBLAS and quad diag = A0Z * A1

maptrees <- function(x) {  
  # Coordinates of the 4 corners of the grid cell
  # A and B could be KnownLeft and KnownRight, but this isn't necessary.
  GridPointAx <- x[1]
  GridPointAy <- x[2]
  GridPointBx <- x[3]
  GridPointBy <- x[4]
  GridPointCx <- x[5]
  GridPointCy <- x[6]
  GridPointDx <- x[7]
  GridPointDy <- x[8]
  
  # Known values
  KnownLeftX <- x[9]
  KnownLeftY <- x[10]
  KnownRightX <- x[11]
  KnownRightY <- x[12]
  UnknownToLeft <- x[13]
  UnknownToRight <- x[14]
  
  # Describe two circles of known radii and center and find 2 intersection points
  
  RightToLeft <- ((KnownRightX - KnownLeftX)^2 + (KnownRightY - KnownLeftY)^2)^.5
  
  if ((UnknownToLeft + UnknownToRight) < RightToLeft) {
    UnknownX <- NA
    UnknownY <- NA
    UnknownCoords <- c(UnknownX, UnknownY)
    return(UnknownCoords)
    
  }  
  
  MidpointToLeft <- (UnknownToLeft^2 - UnknownToRight^2 + RightToLeft^2) / (2 * RightToLeft)
  MidpointToRight <- (UnknownToRight^2 - UnknownToLeft^2 + RightToLeft^2) / (2* RightToLeft)
  MidpointToUnknown <- (UnknownToLeft^2 - MidpointToLeft^2) ^ .5
  
  MidpointX<- KnownLeftX + ((MidpointToLeft * (KnownRightX - KnownLeftX))/RightToLeft)
  MidpointY<- KnownLeftY + ((MidpointToLeft * (KnownRightY - KnownLeftY))/RightToLeft)
  
  SolutionOneX<- MidpointX+ ((MidpointToUnknown * (KnownRightY - KnownLeftY))/RightToLeft)
  SolutionTwoX <- MidpointX- ((MidpointToUnknown * (KnownRightY - KnownLeftY))/RightToLeft)
  
  SolutionOneY <- MidpointY- ((MidpointToUnknown * (KnownRightX - KnownLeftX))/RightToLeft)
  SolutionTwoY <- MidpointY+ ((MidpointToUnknown * (KnownRightX - KnownLeftX))/RightToLeft)
  
#   Optionally show values
#   SolutionOneX
#   SolutionOneY
#   SolutionTwoX
#   SolutionTwoY

# Choose the solution closest to the center of the grid cell
  GridCellCenterX <- (GridPointAx + GridPointBx + GridPointCx + GridPointDx) / 4
  GridCellCenterY <- (GridPointAy + GridPointBy + GridPointCy + GridPointDy) / 4
  
  SolutionOneDistance <- ((SolutionOneX - GridCellCenterX)^2 + (SolutionOneY - GridCellCenterY)^2)^.5
  SolutionTwoDistance <- ((SolutionTwoX - GridCellCenterX)^2 + (SolutionTwoY - GridCellCenterY)^2)^.5
  
  if (SolutionOneDistance == SolutionTwoDistance) {
    UnknownX <- SolutionOneX
    UnknownY <- SolutionOneY
  } else if (SolutionOneDistance < SolutionTwoDistance) {
    UnknownX <- SolutionOneX
    UnknownY <- SolutionOneY
  } else if (SolutionTwoDistance < SolutionOneDistance) {
    UnknownX <- SolutionTwoX
    UnknownY <- SolutionTwoY
  }

# Return chosen coordinates
  UnknownCoords <- c(UnknownX, UnknownY)
  return(UnknownCoords)
}
