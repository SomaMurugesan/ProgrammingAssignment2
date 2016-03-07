## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function create the Cache of the Inverse of the matrix
##
##  Set the value of the Matrix
##  Get the value of the Matrix
##  Set the value of the Inverse of the Matrix
##  Get the value of the Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
 # Intitalize the Inverse Matrix 
  Invm <- NULL
  set <- function(y) {
    x <<- y
    Invm <<- NULL
  }
  get <- function() x
  setInverse <- function(Solve) Invm <<- Solve
  getInverse <- function() Invm
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function
## It checks from the Cache whether we have the inverse of the Matrix already
## exists in the cache, if exists it returns that inverse.
## If the inverse does not exits in the catche then in compute the inverse using
## the Solve funtion and set the newly computed inverse to the cache and returns
## the inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Invm <- x$getInverse()
  if(!is.null(Invm)) {
    message("getting cached data")
    return(Invm)
  }
  data <- x$get()
  Invm <- Solve(data, ...)
  x$setInverse(Invm)
  Invm
}