## Two functions that when used together will take a square matrix, solve its 
## inverse and then store it in a cache

## ############################### Example ###################################
## > x <- matrix(1:4, nrow=2, ncol=2)
## > x
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > B <- makeCacheMatrix(x)
## > cacheSolve(B)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(B)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## ###########################################################################

## makeCacheMatrix() takes and stores a square matrix and defines some functions
## to be later used by cacheSolve(), which will be returned in an easily
## navigable list

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # Function for resetting cache variables if original matrix changes
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get  <- function() x
    # The setinverse function will store the inverse of the matrix in a
    # separate environment(cache), but only once cacheSolve() calls it with
    # the solved inverse
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m    
    # Returning a list of all the functions that will be available to cacheSolve()
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## cacheSolve() takes makeCacheMatrix() as its input, which is really a list of
## the functions defined in makeCacheMatrix()

## makeCacheMatrix() should first be instantiated with a square matrix before
## calling it with cacheSolve()

cacheSolve <- function(x, ...) {
    # Populating m with the cached inverse, if it exists
    m <- x$getinverse()
    # Checking to see if a matrix has already been solved/cached
    if(!is.null(m)) {
        message("getting cached data")
        # If not NULL, then a cached inverse matrix exists
        # and will be returned, and cacheSolve() will stop here
        return(m)
    }
    ## If no cache exists in m, the following lines will solve and store it
    
    # Getting the matrix originally passed to makeCacheMatrix()
    data <- x$get()
    # Solving the inverse of the matrix
    m <- solve(data)
    # Finally, storing the inverse into the cache
    x$setinverse(m)
    # Return a matrix that is the inverse of 'x'
    m
}
