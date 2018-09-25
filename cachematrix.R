## Definition of a pair of functions to calculate the inverse of a matrix and cache the result

# Definition of function cacheSolve: this function caches the value of the inverse of a given matrix.

cacheSolve <- function(x = matrix()) {
        ## Return a matrix that is the inverse of 'x'
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinverse <- function(solve) m <<- solve
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
    }

# Definition of function makeCacheMatrix: this function returns the cached value of the inverse of a matrix. In case there is no result in cache, it computes the inverse of the matrix.
makeCacheMatrix <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("Getting cached data.")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
