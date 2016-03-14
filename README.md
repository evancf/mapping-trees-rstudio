# mapping-trees-rstudio
This version communicates with my Rstudio.

Repository contents: 

Mapping R files:

cleanmaps2.r: source for "maptrees", an R function that will calculate the GPS coordinates of a tree given its distance from two known grid corner points

runmapping2.r: an (in-progress) script that runs maptrees for all trees in a grid cell and creates a plot of all the trees in a grid cell and its corner points. 

What works: generating coordinates for all the trees in a grid cell, and creating plots with the trees mapped and labeled by tag and the corners labeled.

In progress: Identifying and flagging trees that are likely to be mapped incorrectly; flagging, collecting, and potentially fixing NAs; extracting gps coordinates for the corner points from mapping_temp.csv


Data files for mapping: 

mapping_temp.csv: mapping data for all trees at all sites across all islands.

maptesta2b3.csv: the subset of mapping_temp that contains cell a2b3 in NBLAS on Guam. The columns are unlabeled, but they are: the gps coordinates of the corners of the cell, the gps coordinates of the left and right target points, the distance from each tree to the left and right target points, the tag number, and the species. This is the format that runmapping2.r and maptrees use. 

maptesta0za1.csv: the subset of mapping_temp for a0-z*a1 in nblas, also in the format that runmapping2 and maptrees use.

a0za1corners.csv: gps coordinates for the corners of a0-z*a1 in nblas

a2b3corners.csv: gps coordinates for the corners of a2-b3 in nblas

mapping_nblas.xlsx: the subset of mapping_temp that contains NBLAS on Guam. 

Remaining files:

I've been working on a script that can use the "quaddiag" column of mapping_temp.csv to find the gps coordinates of the four corners of a grid cell, and then use that information to map the trees and create a plot with labeled corners. The rest of these files are support for that project. 

nblas_gps_library.csv: gps coordinates for all the grid points in NBLAS.

nblasgps.xlsx: a table showing the spatial arrangement of the grid points. 

nblas_gps_names.csv: the .csv version of nblasgps.xlsx

possiblenames.csv: a list of the possible names for grid cells.

maphandling.R: source for "getcorners", a function that will find the names of the 4 corner points of a cell from its "quaddiag" value.

runmaphandling.R: script to test getcorners. It runs getcorners on every possible quaddiag value (aka cell name). 
