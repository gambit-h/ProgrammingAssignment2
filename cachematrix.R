## These functions are to complete the Week 3 assignment of the R Programming course
## These functions are used to cache computationally intensive results, in particular
## the inversion of a matrix.

## makeCacheMatrix: a function that stores a matrix and and its inverse

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() {
            x
        }
        setInverse <- function(invMatrix) {
            inv <<- invMatrix
        }
        getInverse <- function() {
            inv
        }
        list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}


## cacheSolve: a function that, for a given 'object' created by the makeCacheMatrix,
## can set the inverse of the matrix found within the object, or returns the
## inverted matrix if the inverted matrix is already stored within the object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getInverse()
    if (!is.null(i)) {
        return(i)
    }
    theMatrix <- x$get()
    i <- solve(theMatrix, ...)
    x$setInverse(i)
    i
}
