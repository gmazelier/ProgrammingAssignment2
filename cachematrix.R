## The two following functions are used to create an object that stores a matrix
## and caches its inverse. You can find usage example with the expected output
## below:
##
## > x <- makeCacheMatrix(matrix(c(4,2,7,6), nrow=2, ncol=2))
## > x$get()
## [,1] [,2]
## [1,]    4    7
## [2,]    2    6
## > x$getInverse()
## NULL
## > cacheSolve(x)
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## > cacheSolve(x)
## getting cached data
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## > x$getInverse()
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## > x$get() %*% x$getInverse()
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > x$set(matrix(c(1,2,3,4), nrow=2, ncol=2))
## > x$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > x$getInverse()
## NULL
## > cacheSolve(x)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > x$getInverse()
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


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
