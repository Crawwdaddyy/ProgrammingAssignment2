## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This will create an object that can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
          x <<- y
          i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list (set = set,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Write a short comment describing this function
#This will check to see if the inverse of the matrix is in the cache.
#If it is and the matrix hasn't change, it will return that invesed matrix in the cache
#If it isn't in the cache or if the matrix has changed, it will inverse and return that matrix


cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
          message("getting cached data")
          return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
