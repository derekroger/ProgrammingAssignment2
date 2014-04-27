## These functions cache the results of inverse matrix calculations.
## Caching calculations allows for the result to be retrieved if it's available rather than re-calculating it.

## This function creates a special vector, which is a list of four functions, that are used to set and get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function retrieves the cached value if it's available, otherwise it computes the inverse matrix.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}