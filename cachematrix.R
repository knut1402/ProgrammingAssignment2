## Aim is to cache the inverse of a matrix. 
## Function 1: creates a special matrix object that 
## can cache its inverse
## Function 2: Computes the inverse ofthe special matrix
## returned by Function 1. If the inverse has already been
## calculated and the matrix has not changed, Function 2 
## aims to retrieve the inverse form the cache

## Function 1

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setmatrixinverse <- function(inverse) inv <<- inverse
    getmatrixinverse <- function() inv
    list(set = set, get = get,
         setmatrixinverse = setmatrixinverse,
         getmatrixinverse = getmatrixinverse)
}


## Function 2

cacheSolve <- function(x, ...) {
  
## Return a matrix that is the inverse of 'x'
    inv <- x$getmatrixinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setmatrixinverse(inv)
    inv
}
