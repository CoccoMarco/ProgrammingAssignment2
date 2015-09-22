## These pair of functions cache the inverse of a matrix
## This is the Programming Assignment 2 for the Coursera's 
## course "R Programming" by prof. Roger Peng.

## This function create a list of four functions: 
## set = set the value of the matrix, 
## get = get the value of the matrix
## setinverse = set the value of the matrix 
## getinverse = get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function computes the inverse of a matrix 
## taking an input the output of the function makeCacheMatrix
## Firstly it checks if the inverse is already available in the cache,
## otherwise it computes the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}