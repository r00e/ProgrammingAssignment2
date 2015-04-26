## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# mackeCacheMatrix is a function which
# creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
#trying to get the inverse, 
#if the inverse is existed, then return it directly
#if the inverse is NOT existed, caculate it and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse){
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
