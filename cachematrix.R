## The following pair of functions cache the inverse of a matrix. You have
## to call first the function makeCacheMatrix() to create an object that will
## be the argument for the function that actually calculates or retrieves the
## inverse matrix.

## makeCacheMatrix creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Creat an object "inv" that will eventually contain the inverse matrix
    # and set it to "NULL"
    inv <- NULL
    # Create the elements that will constitute the output of the function
    # This object will contain 4 elements, all of them functions, "set",
    # "get", "setinv" and "getinv".
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invers) inv <<- invers
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    ## Caution: cacheSolve assumes the matrix is invertible!!!
    inv <- x$getinv()
    # If the inverse has already been calculated cacheSolve retrieves it from
    # the cache and returns the retrieved value
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # Otherwise we create a local matrix "data" that will contain the matrix
    # to be inverted
    data <- x$get()
    # The inverse is calculated using the solve function
    inv <- solve(data, ...)
    # The cached inv is assigned the value of the inverse
    x$setinv(inv)
    # Inverse matrix is returned
    inv
}

