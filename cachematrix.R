## R-prog Assignment 2
##
## A pair of functions that 
## A: Wrap a matrix with some functionality (makeCacheMatrix)
## B: Perform operations on a wrapped matrix (cacheSolve)

## A matrix wrapper that can get and set the contained matrix
## Also provides functions for setting a 'solve' function
## and getting the result of of that function 
## (the function must be set first).

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}



## Operates on a matrix wrapped by the makeCacheMatrix
## Checks to see if the wrapped matrix has already had the 
## solve function set, 
## if so, returns the value returned
## else, sets the solve function, then returns the value

cacheSolve <- function(x, ...) {
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}

