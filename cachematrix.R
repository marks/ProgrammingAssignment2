## This file contains code to create an object that is able to cache a matrix's inverse

## This function creates a special matrix object that has  attributes to get and set both the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverse <<- inverse
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function returns the specified matrix's inverse from cache, if available, and computes and stores the inverse in cache if the cache is not available.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    # if inverse is not null (it was in the cache), return it
    if(!is.null(inverse)) {
        message("getting inverse from cache")
        return(inverse)
    }
    # otherwise, get the matrix's data, compute the inverse, and store it in cache
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse # return the newly computed and stores inversesolv
}


## "Tests"
# m <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
# cm <- makeCacheMatrix(m)
# cacheSolve(cm) # works!
# cacheSolve(cm) == solve(m) # tells me cm$getInverse() has the same value as solve(m)