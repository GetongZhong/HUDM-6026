library(MVA)
data("USairpollution", package = "HSAUR2")
mlab <- "Manufacturing enterprises with 20 or more workers"
plab <- "Population size (1970 census) in thousands"

outcity <- match(lab <- c("Chicago", "Detroit",
                          "Cleveland", "Philadelphia","Houston"), rownames(USairpollution))
x <- USairpollution[, c("manu", "popul")]


data("USairpollution", package = "HSAUR2")
USairpollution


for(i in seq(1, 7)){
  for(j in seq(i, 7)) {
    if (i != j) {
      bvbox(USairpollution[, c(i, j)], xlab=names(USairpollution)[i], ylab=names(USairpollution)[j])
      text(USairpollution[, c(i, j)][,1], USairpollution[, c(i, j)][,2], labels = rownames(USairpollution), cex = 0.7, pos = c(2, 2, 4, 2, 2))
      
    }
  }
}
