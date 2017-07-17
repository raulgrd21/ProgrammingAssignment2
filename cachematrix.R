## This function caches the inverse of a given matrix to save costly computations
## It works by creating a an object that allows access to the complete environment of makeCacheMatrix
## when the functions has stoped running, this enable to store the inverse of the matrix that cacheSolve will
## will calculate only once, and retreive without having to recalculate it

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)

}


## This functions checks if the inverse has been already calculated. If it has, it then retreives it, if it
## hasn't it calculates it and caches it by calling its setter, which in turn stores the new value "inv" in the
## makeCacheMatrix's envirornment allowing its further access without recalculation

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
