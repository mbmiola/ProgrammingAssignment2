## Matrix inversion is usually a costly computation and caching it its better than computing it repeatedly.
## So, this script describes a pair of functions that:
## 	* caches the matrix and its inverse
##  * provides the correct methods to manipulate this information
##	* calculates the matrix inverse, if it is not already defined in the cache

## function makeCacheMatrix
## This functions provides the basic methods to set and retrieve information
## about a matrix and its inverse
##	* get (reads the original matrix)
##	* set (stores the original matrix into the cache)
##	* getinverse (reads the matrix inverse)
##	* setinverse (stores the matrix inverse)
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function(){x}
  getinverse <- function(){m}
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  setinverse <- function(inverse){m <<- inverse}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## function cacheSolve
## This function reads the information stored in the cached matrix object
## - if the inverse is already defined (!null), then
##      * get the inverse from cache and return it
## - else
##	* get the original matrix
##	* calculate the inverse
##	* save the information in cache
##	* return the inverse
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
