## These functions allow us to invert a square invertible matrix 
## using cache facilities.
## Sample of use:
## - creating a square invertible matrix
## z <- cbind(1:2, 4:5)
## - creating a cache matrix
## w <- makeCacheMatrix(z)
## - showing the matrix recently set
## w$get()
## - showing there is no inverted matrix
## w$getinv()
## - calculating the inverse of matrix and caching it
## cacheSolve(w)
## - showing the cached inverted matrix
## w$getinv()
## - showing the function retrieving cached inverse
## cacheSolve(w)

## Use makeCacheMatrix () to create an cached object with the properties 
## mtx (given matrix) and inv (its inverse). There are also four methods to
## set and get those properties: set(), get(), setinv(), getinv().

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


## Use cacheSolve() to cache the inverse of a given matrix 
## if it is not cached yet.

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
    return (inv)
}