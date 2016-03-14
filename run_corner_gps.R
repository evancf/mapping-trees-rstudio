cornernamelist <- c("a2", "b2", "a3", "b3")
cornernamestest <- matrix(data=cornernamelist, nrow=2, ncol=2, byrow=TRUE, dimnames=NULL)

source("corner_gps_function.R")
cornergps(cornernamestest)
