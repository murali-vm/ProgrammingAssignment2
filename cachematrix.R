## Put comments here that give an overall description of what your
## functions do

## This R source file contains 2 functions: 
##  makeCacheMatrix() & cacheSolve()

## The overall purpose of these 2 functions is to cache the result of matrix inversion
## which is a costly computation. If it is either the first time the inversion for a matrix
## is being calculated or if the matrix has been reset, only then the matrix inversion is 
## done via the solve() function. In case, the matrix inversion has already been done once, for
## all successive function calls of the cacheSolve() function, the cached value of the inverse
## is returned using the lexical scoping feature of R.

## Write a short comment describing this function

## This function contains getters and setters for the input matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function

## This function returns the inverse of the input matrix using the solve() function or the cached value.

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
