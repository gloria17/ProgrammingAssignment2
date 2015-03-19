## makeCacheMatrix, cacheSolve
## Pair of functions to handle special "matrix" object that can cache its 
## inverse

makeCacheMatrix <- function(m.x = matrix()) {
    # Creates a special "matrix" object that can cache its inverse.
    #
    # Args:
    #   x: a matrix whose inverse is to be computed and cached.
    #
    # Returns:
    #   a list object implementing the CacheMatrix
 
    print("makeCacheMatrix")
    
    m.inv <- NULL
    set <- function(y) {
        m.x <<- y
        m.inv <<- NULL
    }
    get <- function() m.x
    
    setinv <- function(inv) m.inv <<- inv
    
    getinv <- function() {
        m.inv
    }
    
    print(objects())
    
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}



cacheSolve <- function(x, ...) {
    # Computes the inverse of the special "matrix" returned by makeCacheMatrix 
    # above. If the inverse has already been calculated (and the matrix has not 
    # changed), then the cachesolve should retrieve the inverse from the cache
    #
    # Args:
    #   x: a CacheMatrix object.
    #
    # Returns:
    #   the inverse matrix of x    
    
    y <- x$getinv()
    if(!is.null(y)) {
        # inverse of matrix is already compuited and cached
        # return it
        message("getting cached data")
        return(y)
    }
    # get the matrix to be inversed
    data <- x$get()
    # compute the inverse
    y <- solve(data, ...)
    # cache it
    x$setinv(y)
    y
}

##
## Test data:
##        [,1] [,2]
##   [1,]    2   -1
##   [2,]   -1    1
##
## Inverse:
##        [,1] [,2]
##   [1,]    1    1
##   [2,]    1    2
##

m <- matrix(c(2,-1,-1,1), c(2,2))
print(m)
print(solve(m))
cm <- makeCacheMatrix(m)



