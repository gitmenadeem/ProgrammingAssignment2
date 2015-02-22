## For caching the inverse of a matrix and returning the cached value if the inverse is already calculated or solving the inverse if not.

## The following function creates a special matrix object that cache its inverse.
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

## The following function computes the inverse of the matrix returned by the above function. If the inverse is already calculated, it will get the inverse from the cache, otherwise it will compute the inverse.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    m
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
