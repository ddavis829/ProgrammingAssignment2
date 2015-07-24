## These functions are to facilitate the caching of an inverse of a matrix
## to increase calculation performance of repeated tasks


## makeCacheMatrix creates a container to store a solved matrix to be used in caching
makeCacheMatrix <- function(x = matrix()) {
    # container to store the solved matrix
    i <- NULL
    
    # function to set the unsolved matrix to x and clear cache
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # function to retrieve the original matrix
    get <- function() x
    
    # function to set the cache to the solved matrix
    setinverse <- function(inv) i <<- inv
    
    # function to retrieve the cached value(will return NULL if not already set)
    getinverse <- function() i
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve encompasses the cached object
## it will return the cached solved matrix, if available
## if the cached solved matrix is not available, it will solve the original matrix,
## store it in cache, and return the solved matrix

cacheSolve <- function(x, ...) {
    # retrieve the cached value of the solved matrix
    i <- x$getinverse()
    
    # check if the cached value is valid by a NULL check
    if (!is.null(i)) {
        # value was already cached, return the value
        message("Getting cached data")
        return (i)
    }
    
    # value was not cached, get the original matrix and store the solved matrix in i
    i = solve(x$get(), ...)
    
    # set the cache to the value of i for subsequent retrieval
    x$setinverse(i)
    
    # return the solved matrix
    i
}
