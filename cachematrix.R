# Caching the Inverse of a Matrix
 
# Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly 

# This functions cache the inverse of a matrix.
 
# the following functions:
#   makeCacheMatrix: This function creates a special "matrix" object with 4 functiones that can cache its inverse.
#   cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve 
# the inverse from the cache.

# Computing the inverse of a square matrix is done with the solve function in R. 
# If X is a square invertible matrix, then solve(X) returns its inverse.

# We assume that the matrix supplied is always invertible.


makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
    
        # define four functions; getMatrix, setMatrix, setInverse, getInverse 
        setMatrix  <- function(y) {
            x <<- y
            inv <<- NULL
        }
        getMatrix  <- function() {
            x
        }          
        setInverse <- function(inverse) {
            inv <<- inverse
        }            
        getInverse <- function() {
            inv
        }
        
        # creates a list with four functions
        list(setMatrix  = setMatrix, 
             getMatrix  = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}
 

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve the 
# inverse from the cache

cacheSolve <- function(x, ...) {

    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # data is a square invertible matrix, solve(data) returns its inverse.
    data <- x$getMatrix()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}

