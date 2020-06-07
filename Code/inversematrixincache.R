##This is how we can get inverse of each value of matrix i.e. 2 -> 0.5
## solve() calculates the inverse of any matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(inverse) m <<- inverse
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}

x <-  matrix(c(1,2,3,4),2,2)
x

newX<- makeCacheMatrix(x)
cachemean(newX)
