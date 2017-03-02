#TODO: This script contains 2 functions and an application
#      The 'storage'-function 'makeCacheMatrix' provides tools to cache input
#      The 'inverting'-function 'cacheSolve' is a wrapper around 'solve' that uses the tools 
#      generated in 'makeCacheMatrix' to compute and cache the inverse of a matrix 

## makeCacheMatrix takes a matrix as an argument and generates a list containing 4 functions
## 'set' and 'get' deal with the matrix input, 'setinverse' and 'getinverse' deal with the inverse of said matrix
##      set: writes input to cache
##      get: retrieves from cache
##      setinverse: writes input to cache
##      getinverse: retrieves from cache


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        returnlist <- list(set = set, 
                           get = get,
                           setinverse = setinverse,
                           getinverse = getinverse)
        return(returnlist)
}


## cacheSolve is a wrapper for solve() that takes the caching tools and data from makeCacheMatrix as input,
##  checks whether the inverse of the matrix is already cached and - only if necessary - 
##  computes and stores it and then returns the inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("retrieving inverse from cache")
                return(inv)
        }
        cachedmatrix <- x$get()
        inv <- solve(cachedmatrix, ...)
        x$setinverse(inv)
        return(inv)
}

## Example

thematrix <- matrix(rexp(64, rate=1), ncol=8)
thechachedmatrix <- makeCacheMatrix(thematrix)

cacheSolve(thechachedmatrix)
cacheSolve(thechachedmatrix)