## makeCacheMatrix creates a special "matrix" that can cache its inverse
## makeCacheMatrix contains functions to "set" the value of the matrix; "get" value
## of the matrix; "setinverse" of the matrix; and "getinverse" of the matrix.  

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
## if the inverse has been calculated the cacheSolve should retrieve the inverse from cache
## cacheSolve uses "getinverse" from makeCacheMatrix to check whether the inverse has exsited.
## cacheSolve uses "get" from makeCacheMatrix to calculate inverse if the inverse has not exsited.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
