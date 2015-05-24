# function makeCacheMatrix returns a list of functions,
# store a matrix and caching the inverse of a matrix.
# function makeCacheMatrix contains functions:
# * setMatr   set a matrix
# * getMatr   get a matrix
# * cacheInv  get the cached inverse of the matrix
# * getInv    get the cached inverse of the matrix
#

makeCacheMatrix <- function(x = numeric()) {
  
  cacheMatr <- NULL                         # holding the cached value or NULL if nothing is cached, initially set it to NULL
  
  
  setMatr <- function(newMatr) {            # store a matrix
    x <<- newMatr
    cacheMatr <<- NULL                      # when the matrix assigned a new value, clear the cache
  }
  
  getMatr <- function() {                   # returns the stored matrix
    x
  }
  
  cacheInv <- function(solve) {             # cache the given argument 
    cacheMatr <<- solve
  }
  
  getInv <- function() {                    # get the cached matrix
    cacheMatr
  }
  
  
  list(setMatr = setMatr, getMatr = getMatr, cacheInv = cacheInv, getInv = getInv)
  # return a list. Each element is a function
}


# function calculates the inverse of matrix created with makeCacheMatrix
cacheSolve <- function(y, ...) {
  
  inverse <- y$getInv()                     # get the cached matrix
  
  if(!is.null(inverse)) {                   # if there a cached value exists - return this matrix
    message("getting cached data")
    return(inverse)
  }
  
  data <- y$getMatr()                       # if not - calculate the inverse of matrix and store it in cache
  inverse <- solve(data)
  y$cacheInv(inverse)
  
  inverse                                   # return the inverse of a matrix 
}