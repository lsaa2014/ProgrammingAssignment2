## These functions cache the inverse of a matrix
## MakeCacheMatrix creates a special "cached matrix", which is really a list containing a function to:
* set the value of the cached matrix
* get the value of the cached matrix
* set the value of the inverse
* get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## store the cached inverse matrix
  set <- function(y) ## Set for the matrix
         x <<- y
         inv <<- NULL
  }
  get <- function() x ## Get the matrix
  setsolve <- function(solve) inv <<- solve ## Set the inverse
  getsolve <- function() inv ## Get the inverse
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}
## cacheSolve returns inverse cached matrix created with the above function
cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get() ## Calculate the inverse
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv    ## Return the matrix that is the inverse of 'x'
}