# Script that includes 2 functions to calculate and cache the inverse of a matrix

# 1. makeCacheMatrix : function that creates a special "matrix" object that can cache its inverse
## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix with the function 'set'
## get the value of the matrix with the function 'get'
## set the value of the inversed matrix with the function 'setinv'
## get the value of the inversed matrix with the function 'getinv'


makeCacheMatrix <- function(x = matrix()) {

    x_inv <- NULL
    
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(x_inversed) x_inv <<- x_inversed
    
    getinv <- function() x_inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



# 2.cacheSolve is a function that computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'

    x_inv <- x$getinv()
    if(!is.null(x_inv)) {
        message("getting cached data")
        return(x_inv)
    }
    data <- x$get()
    x_inv <- solve(data, ...)
    x$setinv(x_inv)
    x_inv
}


m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)


M1 <- makeCacheMatrix(m1)
cacheSolve(M1)

M1$getinv()
