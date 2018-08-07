## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This fuction creates a special "matrix" object that can caches its inverse.
# Usage: "myMatrix <- makeCacheMatrix(matix)".
# "myMatrix" is a list object with mutator and accessor methods
# and matrices.
makeCacheMatrix <- function(x = matrix()) { # Give a none matrix
        inverse <- NULL # Cache inverse state is "NULL" at the function start
        # set(matrix) :mutator
        set <- function(y){
                x <<- y  # Pass set(matrix) to parent frame
                inverse <<- NULL  #Cache inverse state clean
        }
        # get(matrix) :accessor
        get <- function() x
        # setInverse(matrix) :mutator
        setInverse <- function(inverseX) inverse <<- inverseX
        # getInverse(matrix) :accessor
        getInverse <- function() inverse
        # Return a list object with the above four functions
        list(set = set, get = get, 
             setInverse= setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by 
# "makeCacheMatrix()". If the inverse has already been calculated (and the
# relative matrix has not changed), then the "cachesSolve" should retrieve the
# the inverse matrix from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        # Get the matrix from "makeCacheMatrix()" object
        data <- x$get()
        # Compute the inverse data.matrix
        inverse <- solve(data, ...)
        # Set caching inverse matrix by "makeCacheMatrix()"
        x$setInverse(inverse)
        inverse
}
