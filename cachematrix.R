## The cacheMatrix R file has two main functions, makeCacheMatrix and cacheSolve. 
## These two functions store, calculate and/or retrieve the the inverse of an input matrix (if already calculated) to/from the cache.

## The makeCacheMatrix contains four sub functions. They are: set, get, setMatrix and getMatrix.
## The functions serve the following purpose:
##    set       : Stores the input x (of type matrix) in the cache
##    get       : Retrieves the matrix from the cache
##    setMatrix : The inverse matrix of the input x is calculated and then stored in the cache.
##    getMatrix : The inverse matrix of the input x, if available in the cache, is retrieved.
## The result of the four functions explained above are finally stored in a list.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(solve) m <<- solve
  getMatrix <- function() m
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}

## The cacheSolve function uses the getMatrix function to determine whether the inverse of the input matrix (x) has been calculated before and available in the cache. 
## If it is available in the cache, it is retrieved, otherwise the inverse is calculated, stored in the cache and displayed.

cacheSolve <- function(x, ...) {
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix(m)
  m
}