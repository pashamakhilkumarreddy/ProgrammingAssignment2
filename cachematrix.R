## Functions to compute the inverse of a matrix

## The function below created a special matrix object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
        get  <- function () x
        setInverse <- function (solveMatrix) inv <<- solveMatrix
        getInverse <- function () inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function below is to compute the inverse of the special matrix returned by the makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message('Getting cached data')
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
