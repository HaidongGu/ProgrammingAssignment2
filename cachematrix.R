## The are two functions in in this R file: makeCacheMatrix and 
## cacheSolve. They are used to create a special object that 
## stores a matrix and cache's its inverse, which could be 
## potentially time-consuming.

## makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function calculates the inverse of the 
## special "matrix" created with makeCacheMatrix(). 
## However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setInverse 
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
