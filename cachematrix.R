## The 2 functions below are submitted in fulfiment of requirements for
## Assignment 2 for the R Programming course in coursera
## The functions are used to create a matrix & store its inverse in
## cache. The inverse is caluclate only once.

## makeCacheMatrix creates a matrix and stores its inverse in cache

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL ## initiate NULL inverse matrix
    set <- function(y) { ## implement the matrix set method
        x <<- y 
        inv <<- NULL ## when matrix is set, its inverse is NULLed
    }
    get <- function() x ## implement the matrix get method
    setinv <- function(invmat) inv <<- invmat ## implement matrix inv set method
    getinv <- function() inv ## implement matrix inv get method
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve accepts a matrix, then checks whether its inv was previously
## computed (by checking cache). If yes, it skips calculation, if not, it
## computes it.

cacheSolve <- function(x, ...) {
    inv <- x$getinv() ## retrieve cached inv
    if(!is.null(inv)) { ## if cached inv exists, retrieve it & exit
        message("getting cached data")
        return(inv)
    }
    data <- x$get() ## if no inv cached, retrive the matrix
    inv <- solve(data, ...) ## calculate the inverse
    x$setinv(inv) ## cache the inverse
    
        ## Return a matrix that is the inverse of 'x'
    inv
}
