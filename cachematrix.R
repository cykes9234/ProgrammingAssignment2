## Caching the Inverse of a Matrix:
## The process of matrix inversion is usually a costly computation.
## Therefore, it is beneficial to cache the inverse of a matrix
## instead of computing it repeatedly.
## The two functions below can be used together to create a special object
## which stores a matrix and then caches is inverse.

## This function is used to create a special object, in the form of a matrix
## that can cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" object that is
## created by the makeCacheMatrix. If the inverse is calculated (and the matrix
## stays the same), then it will retrieve  the inverse from the cache from earlier.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
