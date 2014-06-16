## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly.

## makeCacheMatrix(x) creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve(x, ...) computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.
## Assumes the matrix supplied is invertible.

## For example, try this:
## mymat <- matrix(c(4,0,0,8), 2, 2)
## mycachedmat <- makeCacheMatrix(mymat)
## cacheSolve(mycachedmat)  # executes solve()
## cacheSolve(mycachedmat)  # gets cached data
## mymat <- matrix(c(2,1,1,3), 2, 2)
## mycachedmat <- makeCacheMatrix(mymat)
## cacheSolve(mycachedmat)  # executes solve()
## cacheSolve(mycachedmat)  # gets cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv        
}
