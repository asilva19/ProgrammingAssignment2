## This pair of functions work together to compute the inverse of a matrix and to store that inverse into a
## cache, removing the need to re-compute the inverse if it has already been done before.



## This function wraps several helper functions into a single list. These helper functions do the underlying
## work to set and to get the inverse of the matrix to and from the cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function actually returns the inverse, either from the cache or by computing it directly, and then
## saves the inverse to the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}