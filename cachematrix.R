## Helper functions to produce and cache the inverse of an invertible square matrix

## Create a matrix wrapper that supports setting/retrieval of the inverse matrix
## Args:
##   x: An invertible square matrix; defaults to an empty matrix
# Returns:
#   A wrapped matrix supporting get, set, getinverse and setinverse functions
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Retrieve the inverse of a matrix, retrieving results from cache if possible otherwise calculating
## Args:
##   x: An invertible square matrix as wrapped by makeCacheMatrix function
# Returns:
#   Inverse of matrix x
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
