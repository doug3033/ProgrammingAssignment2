## This program demonstrates a the caching of the inverse of a given matrix so that it will only be computed once.

## This function returns a "special matrix" - essentially a list of functions.  
## The list contains functions to get and set a matrix and get and set the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The funciton takes a matrix and returns the inverse from cache if available, if not, it computes the inverse and
## stores it.  The function argument an "special maxtrix" (an instance of makeCacheMatrix).  


cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m

}
