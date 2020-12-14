## Put comments here that give an overall description of what your
## functions do

## cache the inverse of a matrix

makeCacheMatrix <- function(M = matrix()) {
        inv <- NULL
        set <- function(y) {
                M <<- y
                inv <<- NULL
        }
        get <- function() M
        setinv <- function(z) inv <<- z
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## invert a matrix looking if its already cached

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        M <- x$get()
        inv <- solve(M, ...)
        x$setinv(inv)
        inv
}
