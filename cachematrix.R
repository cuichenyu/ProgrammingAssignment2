## These two functions are used to create a special object that stores a matrix
## and cache's its inverse. If the contents of the matrix are not changing, its
## inverse can be looked up in the cache rather than recomputed.

## This function creates a special "matrix" object that can cache its inverse.
## It is really a list containing a function to 1.set the value of the matrix;
## 2.get the value of the matrix; 3.set the value of the solve; 4.get the value
## of the solve.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setsolve(m)
    m
}