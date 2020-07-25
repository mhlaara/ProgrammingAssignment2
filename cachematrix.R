## These functions create a special matrix and then they give you the inverse 
## of the original matrix. This is done by creating a special object by the 
## makeCaheMatrix function and then returning the inverse of that special object
## by the cacheSolve function


## This function creates a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function calculate the inverse of that matrix. If the inverse is already 
## cached, this function gives the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
    message("getting data from cache")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}