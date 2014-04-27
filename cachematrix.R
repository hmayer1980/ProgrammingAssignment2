## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly (there are also alternatives to matrix inversion that we 
## will not discuss here). 
## The following functions provide support for creating special matrix
## objects that allow to cache the inverse of the matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # initialize inverse to null
    i <- NULL
    # define set accessor for the matrix
    set <- function(y) {
        # store the matrix
        x <<- y
        # store the inverse as NULL
        i <<- NULL
    }
    # define get accessor for the matrix
    get <- function() x
    # define set accessor for the inverse
    setinverse <- function(solve) i <<- solve
    # define get accessor for the inverse
    getinverse <- function() i
    # construct the accessor functions
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix . If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    # get the inverse (maybe NULL)
    i <- x$getinverse()
    # check if the cached inverse is not null
    if(!is.null(i)) {
        # Return the cached inverse
        return(i)
    }
    # if there is no cached value get the data
    data <- x$get()
    ## and compute the inverse
    i <- solve(data, ...)
    # and store the inverse in the object to use it the next time
    x$setinverse(i)
    # return the inverse
    i
}
