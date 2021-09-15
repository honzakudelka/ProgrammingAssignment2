## Put comments here that give an overall description of what your
## functions do

## A constructor function to the "cache" matrix. 
## inv (inverse) is NULL at beginning, because it is not yet computed
## set function sets a new value for the matrix, resetting the inverse value
## get is a get function for the matrix value
## setinverse is a set function for the inverse of the matrix
## getinverse gets the cached inverse of the matrix
## all these functions are stored in a list, returned by the constructor fun.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}



## function applicable to matrices constructed with makeCacheMatrix
## the cached inverse is first fetched from the cache
## if it is not Null it is returned immediately
## otherwise, the inverse is calculated with solve(), cached and returned

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
