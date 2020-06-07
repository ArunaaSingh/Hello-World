rowMeans(iris[, 1:4])

setofFour <- mtcars$cyl == 4
dataset4 <- mtcars[setofFour,]

setofeight <- mtcars$cyl == 8
dataset8 <- mtcars[setofeight,]


head(iris)

str(dnorm)

Rprof()

sample(1:10)

set.seed(1)
rpois(5, 2)

set.seed(10)
x <- rep(0:1, each = 5)
e <- rnorm(10, 0, 20)
y <- 0.5 + 2 * x + e

summaryRprof()

summary.data.frame(dataset4)

library(datasets)
Rprof()
fit <- lm(y ~ x1 + x2)
Rprof(NULL)

summaryRprof(lm)


