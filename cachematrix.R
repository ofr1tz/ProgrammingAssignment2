## Welcome! The following functions are my attempt of solving
## assignment 2 of the R Programming course.

## makeCacheMatrix creates a "special matrix" which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      cache <- NULL
      set <- function(y) {
            x <<- y
            cache <<- NULL
      }
      get <- function() x
      setinv <- function(inv) cache <<- inv
      getinv <- function() cache
      list(set = set, 
           get = get,
           setinv = setinv,
           getinv = getinv)
}


## cacheSolve checks if there is an inverse matrix in the cache. 
## If not so, it creates and caches the inverse matrix.

cacheSolve <- function(x, ...) {
      cache <- x$getinv()
      if(!is.null(cache)) {
            message("getting cached data")
            return(cache)
      }
      data <- x$get()
      cache <- solve(data, ...)
      x$setinv(cache)
      cache
      
      ## Return a matrix that is the inverse of 'x'
}
