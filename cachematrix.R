## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" with four functions:
## * `set` sets the matrix to inverse and the cached result to NULL
## * `get` returns the current value of the input matrix
## * `setInverse` sets the inverse of the current matrix
## * `getInverse` returns the cached inverse matrix if available, NULL otherwise
makeCacheMatrix <- function(x = matrix()) {
    cached.inverse <- NULL
    set <- function(y) {
        x <<- y
        cached.inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cached.inverse <<- inverse
    getInverse <- function() cached.inverse
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Computes the inverse of a special "matrix" created with `makeCacheMatrix`. If
## the inverse is already computed (cached), the computation is skiped and the
## cached result is returned. Otherwise, the inverse is computed using the R
## `solve` function, stored as cached value and then returned.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    inverse <- solve(x$get(), ...)
    x$setInverse(inverse)
    inverse
}
