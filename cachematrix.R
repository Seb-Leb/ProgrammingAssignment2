## These two functions allow to cache the inverse of a matrix to avoid repeatedly computing it.

## This function outputs a list object to cache an input matrix in.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function either retrieves the inverse of a matrix from a cache or
## computes the inverse of a matrix and stores it in a cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message('getting cached data')
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
