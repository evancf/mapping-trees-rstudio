# Call file containing function
source("cleanmaps2.R") 

# Test case: result should be -0.95, 1.96
x <- c(-5, 10, 5, 10, 10, 0, -5, 0, -5, 10, 5, 10, 9, 10)
maptrees(x)

# Read data from .csv
# File must have 6 columns: 
# Known left x, Known left y, Known right x, Known right y, Distance to left target, Distance to right target
datain <- read.csv("mappingtest3.csv", header=FALSE)



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

cornersxa <- c(datain[1,1], datain[1,3], datain[1,5], datain[1,7])
cornersya <- c(datain[1,2], datain[1,4], datain[1,6], datain[1,8])
cornerslabsa <- c(1:4)
cornersx <- as.vector(cornersxa, mode="numeric")
cornersy <- as.vector(cornersya, mode="numeric")
cornerslabs <-as.vector(cornerslabsa, mode="numeric")
cornersbind <- cbind(cornersx, cornersy, cornerslabs)
cornerstoplot <- as.data.frame(cornersbind)

forplot <- as.data.frame(dataout[1:28,1:3])
# forplot
visualmap <- ggplot(forplot) + geom_point(aes(UnknownX, UnknownY)) + 
  geom_text(aes(UnknownX, UnknownY, label=Tag)) +
  theme_bw()+
  geom_text(data=cornerstoplot, aes(cornersx, cornersy, label=cornerslabs), size=6)
visualmap

# Add the grid cell corners
cornersxa <- c(datain[1,1], datain[1,3], datain[1,5], datain[1,7])
cornersya <- c(datain[1,2], datain[1,4], datain[1,6], datain[1,8])
cornerslabsa <- c(1:4)
cornersx <- as.vector(cornersxa, mode="numeric")
cornersy <- as.vector(cornersya, mode="numeric")
cornerslabs <-as.vector(cornerslabsa, mode="numeric")
cornersbind <- cbind(cornersx, cornersy, cornerslabs)
cornerstoplot <- as.data.frame(cornersbind)
cornersmapped <-ggplot(data=cornerstoplot, aes(x=cornersx, y=cornersy, label=cornerslabs)) + geom_text(size = 10) + geom_point(size = 2)
cornersmapped
visualmap <- cornersmapped + geom_point(data=forplot, aes(x=UnknownX, y=UnknownY, label=Tag)) +
  geom_text(data=forplot, aes(x=UnknownX, y=UnknownY, label=Tag))
visualmap
