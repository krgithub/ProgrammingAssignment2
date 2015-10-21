## Put comments here that give an overall description of what your
## functions do
## The functions makeCacheMatrix and cacheSolve enable the caching the inverse
## operation on a matrix, which would otherwise be expensive to do repeatedly

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inversem) m <<- inversem
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inversem <- x$getinverse()
    if(!is.null(inversem)) {
        message("getting cached inverse matrix")
        return(inversem)
    }
    data <- x$get()
    inversem <- solve(data, ...)
    x$setinverse(inversem)
    inversem
}
