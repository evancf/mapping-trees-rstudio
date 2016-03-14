names <- read.csv("possiblenames.csv", header=FALSE)
namesuse <- as.matrix(names)

times <- 1

while (times <= 48) {
  usetheseletters <- namesuse[times]
  print(getcorners(usetheseletters))
  times = times + 1
}