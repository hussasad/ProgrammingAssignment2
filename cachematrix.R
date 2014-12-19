## makeCacheMatrix creates a matrix object, which will be
## used by the cacheSolve function , which returns the inverse
## of the original marix. it also caches the results for
## subsequent calls

## creates a cached matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(x_){
        x <<- x_
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse_){
        inverse <<- inverse_
    }
    getInverse <- function() inverse
    list(
        get=get,
        set=set,
        getInverse=getInverse,
        setInverse=setInverse)
}


## takes a parameter x (which is a matrix created by
## makeCacheMatrix) and returns its inverse. the results
## are cached
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        message('cache hit!')
        return (inverse);
    }
    message('cache miss')
    data <- x$get()
    inverse <- solve(data,...)
    x$setInverse(inverse)
    inverse
}
