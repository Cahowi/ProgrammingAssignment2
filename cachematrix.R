## The functions below take the inverse of a matrix and 
## store the value as an object.
## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will return the value,
## and not try to calculate repeatedly.

makeCacheMatrix <- function(x = matrix()) {
## creates a special object with default of NULL if not calculated.
        m <- NULL
        set <- function(y) {
                x <<- y
## caches the special matrix object within function environment.
                m <<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) m <<- inverse
        getinverse <- function() m
## this creates a list to house the functions 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The function cacheSolve returns the inverse of the matrix created by
## the makeCacheMatrix function.
## If the cached inverse is already calculated, the function returns it. If not,
## it computes, caches, and returns it.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        } else {
                data<-x$get()
                m <- solve(data, ...)
                x$setinverse(m)
                return(m)
        }
}
