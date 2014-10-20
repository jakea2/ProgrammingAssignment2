## This pair of functions will invert a matrix and cache the solution.  
## If the same matrix inversion is needed again, the function will pull from the cache.

## The first function sets the variables needed for the next function.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function checks the cache, and returns the cache if found.  
## If not found, the solve function is invoked to invert the matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}