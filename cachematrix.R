## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    mx <- NULL
    setmx <- function(y) { 
        x <<- y
        mx <<- NULL
    }
    getmx <- function() x
    setinverse <- function(inverse) mx <<- inverse
    getinverse <- function() mx
    list(setmx = setmx, 
         getmx = getmx,
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Write a short comment describing this function
#computes the inverse 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mx <- x$getinverse()
    if (!is.null(mx)) {
        message("getting cached data")
        return(mx)
    }
    data <- x$getmx()
    mx <- solve(data, ...)
    x$setinverse(mx)
    mx
}
