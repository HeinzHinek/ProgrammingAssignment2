## This project is a solution for coursera.org R programming course assignment.
## It consist of two functions, "makeCacheMatrix" and "cacheSolve".
## The first function takes a matrix as an argument and returns a vector
## containing two functions which set and get the matrix itself and two
## function serving as a cache for the matrix's invert value.
## The second functions takes the vector created by makeCacheMatrix as an
## argument and returns the inverted value of the matrix. In case the inverted
## value is cached by makeCacheMatrix it returns the cached value, otherwise it
## calculates the inverted matrix and stores it in the makeCacheMatrix's cache.


## makeCacheMatrix takes an invertible matrix as an argument and returns
## a vector consisting of four functions:
## set - sets the value of the matrix
## get - gets the value of the matrix
## setinverse - sets the value of the inverted matrix
## getinverse - gets the value of the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse_m <- NULL
    set <- function(y) {
        x <<- y
        inverse_m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse_m <<- inverse
    getinverse <- function() inverse_m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes a vector created by makeCacheMatrix as an argument and
## returns the inverted value of the matrix stored in it. The inverted value 
## is calculated if there is no cached value, otherwise the cached value is
## returned.
cacheSolve <- function(x, ...) {
    inverse_m <- x$getinverse()
    if(!is.null(inverse_m)) {
        message("getting cached data")
        return(inverse_m)
    }
    data <- x$get()
    inverse_m <- solve(data, ...)
    x$setinverse(inverse_m)
    inverse_m
}
