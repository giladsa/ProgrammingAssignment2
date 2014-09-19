## These are 2 funcitons that were implemented as part of Coursera R course
## the functions anbale caching of matrix to reduce computation 
## functions do

## This function creates a special "matrix" object that can cache its inverse.
##input: x is an inversable matrix
##output: a list that includes fucntions to handle the matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve) ##return the "inverse"special" matrix
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
##input: special matrix that was generated using 'makeCacheMarix'
## output: Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s) ##cache
  s ##return the inverse matrix
}
