x <- list(a=1:5, b= rnorm(10), c= rnorm(100,5))
lapply(x,mean)

x <-1:4
lapply(x, runif)

y <- matrix(1:100, 20,10)
apply(y,2, sum)

apply(y,2, mean)

apply(y,1, mean)

apply(y,1, sum)

tapply(x,f,20)

x <- read.csv(file.choose())
sapply(x, function(x) colMeans(x[, c("Ozone", "Solar.R")], na.rm=TRUE))

