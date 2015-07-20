## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix - This function creates a special "matrix" object that can cache its inverse.
## MakeCacheMatrix has 4 subfunctions :
## set - to set the value of matrix
## get - to get the value of matrix
## setinverse - to set the inverse of matrix
## getinverse - to get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## CacheSolve - This function computes the inverse of matrix and returns the same if it is not already computed. 
##If it is already computed and available in the cache, this function retrieves the inverse matrix from cache and returns it.


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
