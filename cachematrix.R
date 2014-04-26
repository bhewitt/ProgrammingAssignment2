## These functions calculate the inverse of a matrix and cache it. If the
## inverse has already been calculated, it gets the value from the cache rather
## than redoing the calculation

## This function creates a list of functions to be called on by the second
## function. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function first checks to see if the inverse cached. If the inverse is
## cached, print the inverse and exit the function, otherwise calculate the
## inverse of the matrix and cache it.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
