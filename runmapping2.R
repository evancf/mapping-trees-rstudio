# Call file containing function
source("cleanmaps2.R") 

# Test case: result should be -0.95, 1.96
x <- c(-5, 10, 5, 10, 10, 0, -5, 0, -5, 10, 5, 10, 9, 10)
maptrees(x)

# Read data from .csv
# File must have 14 columns: 
# First 8 are x,y coordinates of grid cell corners
# Next 6 are: Known left x, Known left y, Known right x, Known right y, Distance to left target, Distance to right target
datain <- read.csv("maptesta0za1.csv", header=FALSE)

# Read corners coordinates and labels from .csv
corners <- read.csv("a0za1corners.csv")

# How many rows? (= how many times to run function)
repeats <- nrow(datain)

# Create empty matrix to store results. 
dataoutrows <- c(1:repeats)
dataoutcols <- c("UnknownX", "UnknownY", "Tag", "Species")
dataout <- matrix(data = NA, nrow = repeats, ncol = 4, byrow = TRUE, dimnames = list(dataoutrows, dataoutcols))

# Run mapping and store results for every row
sofar <- 1
while (sofar <= repeats) {
datatouse <- as.vector(datain[sofar,1:14], mode="numeric")
iterationresult <- maptrees(datatouse)

dataout[sofar, 1:2] <- iterationresult
dataout[sofar, 3] <- datain[sofar,15]
dataout[sofar, 4] <- datain[sofar,16]
sofar = sofar + 1
}

# Look at results
dataout

# Place mapped trees on a plot
library(ggplot2)
# ggrepel looks like it will help move the labels away from the points.
# I'm still working on getting it to run on my computer.
# library(ggrepel)

forplot <- as.data.frame(dataout[1:repeats,1:3])

visualmap <- ggplot(forplot) + geom_point(aes(UnknownX, UnknownY)) + 
  geom_text(aes(UnknownX, UnknownY, label=Tag)) +
  theme_bw()+
  geom_point(data=corners, aes(cornerx, cornery, label=cornerpt), size=3, color="blue") +
  geom_text(data=corners, aes(cornerx, cornery, label=cornerpt), size=6, color="blue")
visualmap

