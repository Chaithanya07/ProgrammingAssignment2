## This is a R function to create a matrx and cache its inverse.
## The first R function will create a special "matrix" and this function is able
## to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  d <- NULL
  set <- function(y) {
    x <<- y
    d <<- NULL
  }
  get <- function() x
  sinv <- function(SpecialMatrix) d <<- SpecialMatrix
  ginv <- function() d
  list(set = set, get = get,
       sinv = sinv, ginv = ginv)
}


##  This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
  d <- x$ginv()
  if(!is.null(d)) {
    message("getting cached data")
    return(d)
  }
  data <- x$get()
  d <- solve(data)
  x$sinv(d)
  d
        ## Return a matrix that is the inverse of 'x'
}
