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


makeCacheMatrix <- function(m = matrix()) {

        # set the value of inverse_cache to NULL (as default if cacheSolve has not yet been called)
        inverse_cache <- NULL

        # set the matrix in the cache
        matrix_cache <- m
    
        # define four functions; setMatrix, getMatrix, setInverse, getInverse 
        setMatrix  <- function(matrix) {
            matrix_cache <<- matrix
            inverse_cache <<- NULL
        }
        getMatrix  <- function() {
            matrix_cache
        }          
        setInverse <- function(inverse) {
            inverse_cache <<- inverse
        }            
        getInverse <- function() {
            inverse_cache
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
    
    # look in cache
    cache_inverse <- x$getInverse()

    # if cache_inverse is not null, use cached inverse
    if(!is.null(cache_inverse)) {
        message("getting cached data")
        return(cache_inverse)
    }

    # get matrix from cache 
    matrix <- x$getMatrix()
    
    # compute inverse (matrix), put the result in new_inverse
    new_inverse <- solve(matrix)
    
    # set inverse in cache
    x$setInverse(new_inverse)
    new_inverse
}

#
# testdata:
#
# mtx1 <- matrix(data = c(4,2,7,6), nrow = 2, ncol = 2)
# mtx2 <- matrix(data = c(4,2,8,6), nrow = 2, ncol = 2)
#
# cmtx <- makeCacheMatrix(mtx1)
#
# cacheSolve(cmtx)
# cacheSolve(cmtx)   # use cache
#
# cmtx <- makeCacheMatrix(mtx2)
#
# cacheSolve(cmtx)
# cacheSolve(cmtx)   # use cache
#
