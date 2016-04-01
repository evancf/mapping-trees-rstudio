# find_tree_coordinates is a function that takes a table of the gps coordinates of the 
# four corners of a grid cell and a vector of the distance from an unknown point to 
# two known points, and returns the coordinates of the unknown point.
#
# It does this by making two circles centered at known points and finding their
#  two (or one, or zero) intersection points. If there are two intersections,
# it returns coordinates for the point closer to the middle of the grid cell. If 
# any necessary fields (e.g. coordinates for known points, coordinates for grid cell
# corners, or distance to known points) are missing, it returns NA. If there are no
# intersections (because the distances to the two known points add up to the less than
# the length of a straight line connecting the two known points), it returns NA.
#
# make_tree_maps.R uses find_tree_coordinates to locate and plot all the trees within
# a grid cell.
#

find_tree_coordinates <- function(cordat, mapdat) {  
  library(sp)
  Issue <- ""
  # Coordinates of the 4 corners of the grid cell
 # A and B could be KnownLeft and KnownRight, but this isn't necessary.
  GridPointAx <- cordat[1,2]
  GridPointAy <- cordat[1,3]
  GridPointBx <- cordat[2,2]
  GridPointBy <- cordat[2,3]
  GridPointCx <- cordat[3,2]
  GridPointCy <- cordat[3,3]
  GridPointDx <- cordat[4,2]
  GridPointDy <- cordat[4,3]
  
  if(anyNA(cordat)) Issue <- paste(Issue, "Corner_missing", sep="_")
  
  
  # Known values
  KnownLeftX <- mapdat[1]
  KnownLeftY <- mapdat[2]
  KnownRightX <- mapdat[3]
  KnownRightY <- mapdat[4]
  UnknownToLeft <- mapdat[5]
  UnknownToRight <- mapdat[6]
  

  
  # Describe two circles of known radii and center and find 2 intersection points
  RTLadjust <- 0 
  RightToLeft <- ((KnownRightX - KnownLeftX)^2 + (KnownRightY - KnownLeftY)^2)^.5
  # If RightToLeft is unusually small (for now, arbitrarily < 9m), the risk of
  # plotting the tree incorrectly is high, because the circles will be close together
  # and small errors in the radii will translate into large errors in the placement of
  # the tree.
  if (RightToLeft < 9) Issue <- "Small_RtoL"
  
  # If sum of the distances to the known left and right points is less than the
  # length of a straight line conencting the two known points, the two circles 
  # will not intersect. 
  # Adjust both distances so they just exceed the length of the straight line
  # connecting the two points. Add a note to Issue. 
  if ((UnknownToLeft + UnknownToRight) < RightToLeft) {
    # UnknownX <- NA
    # UnknownY <- NA
    RTLadjust <- 1
    RTL_change <- (RightToLeft - UnknownToLeft - UnknownToRight) / 2
    UnknownToLeft <- RTL_change + UnknownToLeft + 0.01
    UnknownToRight <- RTL_change + UnknownToRight + 0.01
    Issue <- paste(Issue, "Distances_too_small", sep="_")
    # UnknownCoords <- c(UnknownX, UnknownY, Issue)
    # return(UnknownCoords)
    
  }  
  
  MidpointToLeft <- (UnknownToLeft^2 - UnknownToRight^2 + RightToLeft^2) / (2 * RightToLeft)
  MidpointToRight <- (UnknownToRight^2 - UnknownToLeft^2 + RightToLeft^2) / (2* RightToLeft)
  MidpointToUnknown <- (abs(UnknownToLeft^2 - MidpointToLeft^2)) ^ .5
  
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
  
  # If one of the distances is NA, use the other one. If both are NA, return NA. 
  # The NAs seem to happen when the right to left or unknown to corner distances
  # don't work out. This only happens for points that already had another known issue,
  # like a small RtoL distance or radii that never intersect.
  #
  # If the two solutions are the same distance from the middle, arbitrarily use the
  # first solution. I think this happens when there is exactly one intersection point.
  # 
  # Otherwise pick whichever solution is closer to the middle.
  if ((is.na(SolutionOneDistance) && (is.na(SolutionTwoDistance)))) {
    UnknownX <- NA
    UnknownY <- NA
    Issue <- paste(Issue, "Calculation_problem", sep = "_")
    UnknownCoords <- c(UnknownX, UnknownY, Issue)
    return(UnknownCoords)
  }
  
  if (is.na(SolutionOneDistance)) {
      UnknownX <- SolutionTwoX
      UnknownY <- SolutionTwoY 
      UnknownCoords <- c(UnknownX, UnknownY, Issue)
      return(UnknownCoords)
  }
  
  if (is.na(SolutionTwoDistance)) {
      UnknownX <- SolutionOneX
      UnknownY <- SolutionOneY 
      UnknownCoords <- c(UnknownX, UnknownY, Issue)
      return(UnknownCoords)
  }
  
  if (SolutionOneDistance == SolutionTwoDistance) {
    UnknownX <- SolutionOneX
    UnknownY <- SolutionOneY
    inside <- point.in.polygon(UnknownX, UnknownY, c(GridPointAx, GridPointBx, GridPointCx, GridPointDx, GridPointAx), c(GridPointAy, GridPointBy, GridPointCy, GridPointDy, GridPointAy))
    if (inside == 0) {
      Issue <- paste(Issue, "Outside_cell", sep = "_")
    } else if (RTLadjust == 0) Issue <- "Okay"
  }  
  if (SolutionOneDistance < SolutionTwoDistance) {
    UnknownX <- SolutionOneX
    UnknownY <- SolutionOneY
    inside <- point.in.polygon(UnknownX, UnknownY, c(GridPointAx, GridPointBx, GridPointCx, GridPointDx, GridPointAx), c(GridPointAy, GridPointBy, GridPointCy, GridPointDy, GridPointAy))
    if (inside == 0) {
      Issue <- paste(Issue, "Outside_cell", sep = "_")
    } else if (RTLadjust ==0 ) Issue <- "Okay"
    
  } 
  if (SolutionTwoDistance < SolutionOneDistance) {
    UnknownX <- SolutionTwoX
    UnknownY <- SolutionTwoY
    inside <- point.in.polygon(UnknownX, UnknownY, c(GridPointAx, GridPointBx, GridPointCx, GridPointDx, GridPointAx), c(GridPointAy, GridPointBy, GridPointCy, GridPointDy, GridPointAy))
    if (inside == 0) {
      Issue <- paste(Issue, "Outside_cell", sep = "_")
    } else if (RTLadjust ==0 ) Issue <- "Okay"
  }

  
  # Return chosen coordinates
  UnknownCoords <- c(UnknownX, UnknownY, Issue)
  return(UnknownCoords)
}
