## The makeCacheMatrix function takes a matrix as input and returns a list with 
## functions to obtain the inverse.
## The cacheSolve function returns an inverse of the input matrix.

## Input: An invertible matrix
## Output: A list of functions to
##          1. set the matrix
##          2. get the matrix
##          1. set the inverse
##          1. get the inverse
makeCacheMatrix <- function(x = matrix()) {
    
    ans <- NULL         # set inverse of a new matrix to NULL
    
    set <- function(y) {
        x <<- y
        ans <<- NULL   # if matrix is changed, set inverse to NULL
    }
    
    get <- function(){
        x
    }
    
    setinverse <- function(inverse){
        ans <<- inverse
    }
    
    getinverse <- function() {
        ans
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of the matrix created with the makeCacheMatrix
## function. It first checks the cache if the inverse has already been calculated
## & if true, skips the calculation and returns the cached value. If false, it stores
## the calculated value into cache & returns it.
cacheSolve <- function(x, ...) {
    
    ans <- x$getinverse()
    
    if(!is.null(ans)) {
        message("getting cached data")
        return(ans)
    }
    
    data <- x$get()
    ans <- solve(data, ...)
    x$setinverse(ans)       # Store into cache
    ans
}
