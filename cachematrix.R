## This script contains 2 functions that can be used to compute the inverse
## of an invertible matrix and caches it for future use

## this function creates a special "matrix" object that is actually
## a list with 4 functions to set and get a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## this function computes the inverse of the special "matrix" returned by makeCacheMatrix
## if the inverse has already been calculated then it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

