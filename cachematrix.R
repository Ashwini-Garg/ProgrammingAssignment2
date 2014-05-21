## This file contains two functions -
##  1. The first creates a special matrix object that is capable of caching its inverse.
##  2. The second function returns the inverse of the matrix


## The following function, makeCacheMatrix creates a special matrix object,
## It returns a list containing pointers to the following functions that are 
## nested within this function
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse of the matrix
##   4. get the value of the inverse of the matrix
makeCacheMatrix <- function(currentMatrix = matrix()) {
    inverseOfMatrix <- NULL
    set <- function(y) {
        currentMatrix <<- y
        inverseOfMatrix <<- NULL
    }
    get <- function() currentMatrix
    setInv <- function(calculatedInv) inverseOfMatrix <<- calculatedInv
    getInv <- function() inverseOfMatrix
    list(set = set, get = get,
         setInverse = setInv,
         getInverse = getInv)
}


## The following function takes the cache matrix object created with the
## makeCacheMatrix function as its input and returns the inverse of the matrix.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache. 
## Otherwise, it calculates the inverse of the matrix and sets it in the cache.
cacheSolve <- function(cacheMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- cacheMatrix$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- cacheMatrix$get()
    inv <- solve(data, ...)
    cacheMatrix$setInverse(inv)
    inv
}
