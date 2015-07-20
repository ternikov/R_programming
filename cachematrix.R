## Following functions store and calculate an inverse matrix

## Creating a cache matrix for inversing

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() return(x)
  setinv <- function(inv) inverse <<- inv
  getinv <- function() return(inverse)
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}

## Inverse a cache matrix

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  invserse <- solve(data, ...)
  x$setinv(inverse)
  inverse
}