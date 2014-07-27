## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This is a function that creates the inverse of a matrix
## The return value is the inverted matrix based on an input matrix.

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


## Write a short comment describing this function
## This function uses the makeCacheSolve function to get
## an inverse of a matrix. It saves this value to a cache
## If the value already exists, then this value is used
## rather than running the function again

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
