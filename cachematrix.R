## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mtx = matrix()) {
    inv <- NULL
    set <- function(y) {
        mtx <<- y
        inv <<- NULL
    }
    get <- function() mtx
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(mtx, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- mtx$getinv()
    if(is.null(inv)) {
        data <- mtx$get()
        inv <- solve(data)
        mtx$setinv(inv)
    } else {
        message("getting cached data")
    }
    return inv
}
