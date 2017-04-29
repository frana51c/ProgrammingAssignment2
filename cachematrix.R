## The makeCacheMatrix function caches the inverse of a matrix.
## The CacheSolve function returns the inverse of a matrix, first checking
## whether the result has already been cached by the makeCacheMatrix function.

## The following makeCacheMatrix function gets as argument an invertible
## matrix. It returns a list of functions:
## makeCacheMatrix$set sets the value of the matrix, and resets the cache
## in the global environment.
## makeCacheMatrix$get gets the value of the matrix.
## makeCacheMatrix$setinverse caches the inverse of the matrix in the global
## environment.
## makeCacheMatrix$$getinverse returns the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following cacheSolve function first gets the inverse of the matrix
## from the cache (if present).
## If it is present it returns it without calculating it again and provides
## the message that it has retrieved the cached result.
## If it is not present it gets the value of the matrix with the
## makeCacheMatrix$get function, then calculates its inverse, caches it
## using the makeCacheMatrix$setinverse function and then returns it.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
