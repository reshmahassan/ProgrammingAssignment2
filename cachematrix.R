## The following functions demonstrate how the inverse of matrix, a relatively costly
##computation, can be cached so that we do need to compute it each time we need it.

## makeCacheMatrix function caches the input matrix and inverse matrix and also returns the 
## input matrix and its inverse


makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  ## Caches the input matrix and intializes the inverse matrix to null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##returns the input matrix
  get <- function() {
     x
  }
  
  # caches the inverse matix with the supplied value
  setInverse <- function(inv) {
    m <<- inv
  }
  
  ##returns the inverse of the input matrix
  getInverse <- function() {
     m
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve function checks if the inverse is already in the cache and if it exists will return 
## the cached data. If it doesnt exists,then it will compute the inverse and returns the 
## inverse as well as call the setInverse method to cache it.

cacheSolve <- function(x, ...) {

  m <- x$getInverse()
  
  if(!is.null(m)) {
    message("Inverse of the given Matrix(from the cache):")
    return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  
  x$setInverse(m)
  print("Inverse of the given Matrix:")
  m
  
}



