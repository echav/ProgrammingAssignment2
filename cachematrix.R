## Functions in this file are providing a wrapper around the matrix inversion 
## operations with a caching functionality.
##
## Example usage:
##
## 1. wrap a 1000x1000 matrix:
## cm <- makeCacheMatrix(matrix(rnorm(1000000), nrow=1000, ncol=1000)) 
##
## 2. computation of the inverse, will take some time (cache is empty):
## s1 <- cacheSolve(cm)
##
## 3. try again, will use the cache, very fast:
## s2 <- cacheSolve(cm)
##

## Wrap a matrix (x parameter) in a structure that will also cache the value of
## its inverse after the first time it is computed.
makeCacheMatrix <- function(x = matrix()) {

        cached <- NULL # cache for the inverse once it's computed
        
        # use to update the matrix in the wrapper
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # get the original matrix that is stored in the wrapper
        get <- function() x
        
        # update the value of the cached inverse
        setsolve <- function(solve) cached <<- solve

        # obtain the value of the cached inverse
        getsolve <- function() cached
        
        # available functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Get the inverse of a wrapped matrix, using its cached value if possible.
## X should be a wrapped matrix, and additional optional parameters are the
## same as for the solve() function.
cacheSolve <- function(x, ...) {
        # get the cached value of the inverse
        cached <- x$getsolve()
        # if it is defined, use it
        if(!is.null(cached)) {
                message("getting cached data")
                return(cached)
        }
        # if it is not yet cached, compute it...
        data <- x$get()
        cached <- solve(data, ...) 
        # ...and cache it
        x$setsolve(cached)
        cached
}
