## This file contains two function used for computing the inverse of ma matrix,
## and caching the result in order to save on computation. Below are the
## example functions given in the homework description for finding the
## mean of a vector and caching it in order to be reused without being
## computed again.

## Given examples from instructions:

##makeVector <- function(x = numeric()) {
##  m <- NULL
##  set <- function(y) {
##    x <<- y
##    m <<- NULL
##  }
##  get <- function() x
##  setmean <- function(mean) m <<- mean
##  getmean <- function() m
##  list(set = set, get = get,
##       setmean = setmean,
##       getmean = getmean)
##}

##cachemean <- function(x, ...) {
##  m <- x$getmean()
##  if(!is.null(m)) {
##    message("getting cached data")
##    return(m)
##  }
##  data <- x$get()
##  m <- mean(data, ...)
##  x$setmean(m)
##  m
##}


## This first function creates a list of instructions of functions to
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the value of the inverse
## 4. get the value of the inverse!

makeCacheMatrix <- function(x = matrix()) {
  matInv <- NULL
  set <- function(y){
    x <<- y
    matInv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) matInv <<- inverse
  getInverse <- function() matInv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## The function below calculates the inverse of the matrix, but checks
## if it has been calculated previously before doing so, otherwise
## it uses the inverse stored in cache instead of calculating again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matInv <-x$getInverse()
  if (!is.null(matInv)){
    message("getting cached data")
    return(matInv)
  }
  data <- x$get()
  matInv <- solve(data, ...)
  x$setInverse(matInv)
  matInv
}
