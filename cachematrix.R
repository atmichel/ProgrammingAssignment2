
## These functions input an invertible matrix, create a special "matrix"
## that is capable of calculating or caching the matrix inverse, and 
## returning the matrix inverse that is either calculated or obtained from
## the cache.

## makeCacheMatrix initializes the special "matrix".  Input, x, is a square,
## non-singular matrix:

makeCacheMatrix <- function(x = matrix(numeric())) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve either calculates the inverse of the special "matrix" stored 
## in the output of makeCacheMatrix or obtains the inverse from the cache
## if the inverse has previously been calculated

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data for matrix inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}