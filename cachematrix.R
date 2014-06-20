## This pair of functions cache the inverse of a matrix

## makeCacheMatrix function creates a special matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.  It retrieves the
## inverse from cache if calculation has already been completed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}