## This will contain 2 functions, one to create a matrix that would be able to cache its own inverse,
## and a corresponding one that will allow the inverse to get computed for the matrix created previously.

## This function creates a matrix that would be able to cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setx <- function(inverse) m <<- inverse
        getx <- function() m
        list(set = set, get = get,
             setx = setx,
             getx = getx)
}


## This function actually calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getx()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mat.data <- x$get()
        m <- solve(mat.data, ...)
        x$setx(m)
        return(m)
}
