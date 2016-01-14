## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # define empty inverse matrix
    inv <- NULL
    set <- function(y) {
        # reset X matrix and inversed matrix
        x <<- y
        inv <<- NULL
    }
    #return X matrix
    get <- function() x
    #set and get inversed matrix
    setinv <- function(inver) inv <<- inver
    getinv <- function() inv
    
    # return the list of methods
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## if inversion is already processed and cached
    inv <- x$getinv()
    if(!is.null(inv)){
        message("returning chached inversion")
        return(inv)
    }
    ## if inversion is not yet processed, inverse matrix and return it
    inv <- solve(x$get())
    x$setinv(inv)
    inv
}