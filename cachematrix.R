## The functions here do two things. 1. create a 'special' type of matrix
## with caching abilities, and 2. set and return cached values


## makeCacheMatrix takes in a regular old matrix, x, and creates a
## 'special' matrix that allows the inverse of x to be cached so it
## does not have to be recalculated over and over
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve attempts to fetch the cached inverse of a matrix. if that
## inverse has not yet been calculated, it calculates the value and
## stores it away so that the next time we need the inverse, we can
## just pull up the cached value
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
