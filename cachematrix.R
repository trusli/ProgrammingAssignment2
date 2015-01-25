makeCacheMatrix <- function(x = matrix()) {
    # Cache a matrix for performance purpose
    #
    # Args:
    #   x: a matrix that needs caching
    #
    # Returns:
    #   a list containing the cached matrix along with
    #   set and get functions

    m <- NULL
    
    # cache the matrix in memory
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # provide set and get methods to access the cached matrix
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    
    # return value
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    # Solve a matrix
    #
    # Args:
    #   x: matrix needs to be solved
    #
    # Returns:
    #   solved matrix
    
    # access the cached matrix
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    
    # inverse the matrix
    m <- solve(data, ...)
    
    # set the inverset matrix in memory
    x$setInverse(m)
   
    # return solved matrix
    m
}
