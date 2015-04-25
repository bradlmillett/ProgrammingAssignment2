##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                ## Need to set the object null otherwise cacheSolve wont validate is.null
                m <<- NULL
        }
        ## gets the value of the matrix
        get <- function() x
        ## cache inverse of matrix in m 
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        ## Makes list of functions available
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## get the inverse matrix and assign it to m
        m <- x$getinverse()
        ## if m is not null, get the data from the cache 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if m is null, calculate the inverse matrix
        ## get matrix (x), assign it to data
        data <- x$get()
        ## calculate the inverse matrix
        m <- solve(data, ...)
        ## set the inverse matrix
        x$setinverse(m)
        ## return the inverse matrix
        m
}
