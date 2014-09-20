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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
