## Function Summary:
## This R.script contains 2 functions that will create and stores the inverse
## of a matrix and caches it for quicker retrieval from cache, without 
## requiring recalculation actions

## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse. 
## The matrix must be an 'invertible', square matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve: 
## Return a matrix that is the inverse of 'x'
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve will retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()               #query the cache of the matrix m
  if(!is.null(m)) {                 #if there is a value stored in the cache, then ...
    message("getting cached data")
    return(m)                       #just return the cache, no computation needed
  }
  data <- x$get()                   #if there's no cache ...
  m <- solve(data, ...)             #compute the inverse of the matrix using 'solve'
  x$setInverse(m)                   #save the result back to x's cache for next time
  m                                 #return the result
  }

  