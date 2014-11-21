## cachematrix.R
## -------------
## By: John Williams 11/20/14
## Purpose: 
##      2 functions demonstrate caching of data, which can vastly improve performance when
##          performing operations on large data sets. In this example, the inverse of a
##          matrix is cached.
## 

## Function: makeCacheMatrix
## -------------------------
## Purpose: get and set matrix and its inverse
## Parameter: square matrix data
## Assumptions: supplied matrix is invertible

makeCacheMatrix <- function(x) {
    xInverse <- NULL
    set <- function(y) {
        x <<- y
        xInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) xInverse <<- solve
    getInverse <- function() xInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Function: cacheSolve
## --------------------
## Purpose: Process a passed function containing a matrix and the inverse
##  If the inverse data is cached, return the cached data;
##      otherwise call the solve() function on the matrix, cache and return the data
## Parameter: a makeCacheMatrix function

#The next line is my sample invertible matrix
# y <- matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xInverse <- x$getInverse()
    if(!is.null(xInverse)) {
        message("getting cached data")
        return(xInverse)
    }
    data <- x$get()
    xInverse <- solve(data, ...)
    x$setInverse(xInverse)
    xInverse
}
