## Put comments here that give an overall description of what your
## functions do

## THIS FUNCTION creates and and saves the inverse of a matrix: it has four functions
##which act as getters and setters.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function calculates the 
##inverse of a matrix but first it tries to retrieve 
##the cached value if it is not there then it moves on to calcaluate the inverse and cache it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
