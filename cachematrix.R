## This R file provide two functions for calculating the inverse of a matrix and cache it.
##
## Usage example: 
##     m <- makeCacheMatrix(matrix(c(1, 0.25, 0.25, 1), 2, 2))
##     cacheSolve(m)


## This function caches the inverse of matrix 'x'
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  getsolve <- function() s
  setsolve <- function(solve) s<<-solve
  list(set=set, get=get, getsolve=getsolve, setsolve= setsolve)
}


## This function calculates the inverse of matrix 'x'.
cacheSolve <- function(x, ...) {
  s <- x$getsolve()

  ## If there is a cached value, return it directly.
  if (!is.null(s)) {
    message("Get solve from cache")
    return(s)
  }
  
  # If there is no cached value, calcute the inverse and save it to cache.
  data <- x$get()
  s<-solve(data, ...)
  x$setsolve(s)
  
  ## Return a matrix that is the inverse of 'x'.
  s
}
