# The functions below are the helper functions to facilitate caching of inverse of a matrix. 
# As matrix inversion a costly operation, there may be benefits to cache the result instead of 
# computing it repeatedly. 

## 
 # Creates a special matrix which has the getter and setter methods for the data and its inverse.
 # This special matrix can store its inverse in the cache (parent environments, most probably global environment) 
 # so that it is not computed repeatedly.
 #
 # @param x Matrix whose inverse is getting cached
 # 
 # @return list of setter and getter methods for matrix data and its inverse 
##

makeCacheMatrix <- function(x = matrix()) {
  set <- function(y) {
    entries <<- y
    inverse <<- NULL
  }
  
  get <- function() entries
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse 
  
  set(x) # perform the initial assignment 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##
 # Calculates the inverse of the matrix if the inverse could not be found in the parent environments (cache). 
 # Caches and returns the inverse if it was calculated for the first time, otherwise returns the inverse from the cache. 
 #
 # @param x The object created using makeCacheMatrix
 # 
 # @return The inverse of the matrix
##

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting the inverse from the cache")
    return(inverse)
  }
  
  entries <- x$get()
  inverse <- solve(entries, ...)
  x$setInverse(inverse)
  inverse
}
