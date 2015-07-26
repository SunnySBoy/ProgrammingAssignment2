##
## The functions 'makeCacheMatrix' and 'cacheSolve'
## provide a caching mechanism for the computation
## of inverse matrices.
## (Based on the provided examples.)
##

##
## This function provides (and returns) a special
## "matrix" in form of a list containing
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the cached inverse matrix
## 4. get the value of the cached inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {
  ## delete previous value
  m <- NULL
  
  ## function to set/hold the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## function to get the matrix
  get <- function() x
  
  ## function to set/hold the inversed matrix
  setsolve <- function(solve) m <<- solve
  
  ## function to get the inversed matrix
  getsolve <- function() m
  
  ## return the "special matrix" in form of a list
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##
## This function computes the inverse of the given
## square matrix. If this particular square matrix
## was computed before the cached inverse of the
## matrix is returned.
##
cacheSolve <- function(x, ...) {
  ## get (the probably NULL) cached inverse matrix
  m <- x$getsolve()
  
  ## check if the cached inverse matrix is NULL
  if(!is.null(m)) {
    ## cached inverse matrix is not NULL and hence
    ## is returned
    message("getting cached data")
    return(m)
  }
  
  ## no cached value present, getting matrix
  message("computing...")
  data <- x$get()
  
  ## compute inverse matrix of 'data'
  ## (assume 'data' holds a square matrix)
  ## (exception handling omitted)
  m <- solve(data, ...)
  
  ## cache inverse matrix
  x$setsolve(m)
  
  ## return a matrix that is the inverse of 'x'
  m
}