## Write a pair of functions that cache the inverse of a matrix.

## Create a special "matrix" object that can cache its inverse by
## 1. setting value of matrix
## 2. getting value of matrix
## 3. setting value of inverse
## 4. getting value of matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse hase already been calculated and the matrix has not changed,
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
