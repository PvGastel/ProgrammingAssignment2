#
# Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly 

# This functions cache the inverse of a matrix.

# the following functions:
#   makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#   cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve 
# the inverse from the cache.

# Computing the inverse of a square matrix is done with the solve function in R. 
# If X is a square invertible matrix, then solve(X) returns its inverse.

# We assume that the matrix supplied is always invertible.


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    set_inv <- function(inverse) inv <<- inverse
    get_inv <- function() inv
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve the 
# inverse from the cache


cacheSolve <- function(x, ...) {
    inv <- x$get_inv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    # if data is a square invertible matrix, then solve(data) returns its inverse.
    inv <- solve(data)
    x$set_inv(inv)
    inv
}
