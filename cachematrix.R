#  Contains the following functions:
#  set -> Sets value of matrix
#  get -> Gets value of matrix
#  setInverse -> sets the inverse of the cached matrix
#  getInverse -> gets the inverse of the cached matrix
#
makeCacheMatrix <- function(x = matrix()) {
  
  # initialize cache
  cachedInverse <- NULL
  
  set <- function(newMatrix) {
    x <<- newMatrix
    
    # flush cache
    cachedInverse <<- NULL
  }
  
  # returns stored matrix
  get <- function() {
    x
  }
  
  # caches the  value passed
  setInverse <- function(inverse){
    cachedInverse <<- inverse
  } 
  
  # gets cached value
  getInverse <- function() {
    cachedInverse
  }
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Calculates the inverse of  matrix created with makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  # get cached value
  invFunc <- x$getInverse()
  
  # when we already have a value...
  if(!is.null(invFunc)) {
    message("getting cached data")
    
  }else{
    
    data <- x$get()
    invFunc <- solve(data, ...)
    x$setInverse(invFunc)
  }
  #return value
  invFunc
}
