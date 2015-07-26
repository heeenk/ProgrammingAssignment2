## The functions below allow for caching of the time-consuming operation
## of calculating the inverse of a matrix. 
## If the matrix has changed, a new inverse will be calculated.


## makeCacheMatrix: 
# This function creates a special "matrix" object that can cache its inverse.
# 
makeCacheMatrix <- function(x = matrix()) {

    # the inverse of the matrix
    i <- NULL

    set <- function(y) {              # set the matrix
        x <<- y
        i <<- NULL
    }
    get <- function() {               # get the matrix
        return(x);
    }
    setInverse <- function(inverse) { # set the inverse
        i <<- inverse
    }
    getInverse <- function() {        # get the inverse
        i
    }

    # a list of the functions of makeCacheMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve: 
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse 
# from the cache.
# 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()

    # if the inverse is available and the matrix 'x' hasn't changed
    if(!is.null(i) & matrixEqual(x, x$get()) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()

    # calculate inverse
    i <- solve(data, ...)

    # cache it
    x$setInverse(i)
   
    # return it 
    i
}

## matrixEqual:
# This function checks to see if matrices are equal.
# source: https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html
# NOTE: this is a helper function.
matrixEqual <- function(x, y)
    is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
