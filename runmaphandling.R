source("maphandling.R")
names <- read.csv("possiblenames.csv", header=TRUE)
namesuse <- as.matrix(names)

rows <- nrow(namesuse)

times <- 1

while (times <= rows) {
  usetheseletters <- namesuse[times]
  print(getcorners(usetheseletters))
  times = times + 1
}
