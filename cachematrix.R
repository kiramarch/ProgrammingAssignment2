## This pair of functions cache the inverse of a matrix, to save computation
## time

## makeCacheMatrix creates a special "matrix" object where we can cache a matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL ## initialize to null -- no inverse is calculated

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() {x} ## called by cacheSolve to store original matrix

    setInv <- function(solve) {inv <<- solve} ## stores inverse using superassnmt

    getInv <- function() inv ## called by cachemean to retrieve inverse

    ## returns list with four elements
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve computes the inverse of the special "matrix" object returned by
## makeCacheMatrix. If the inverse has already been calculated then the cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) { ## input is x, an object created by makeCacheMatrix

    inverse <- x$getInv() ## accesses the object and gets value of inverse

    if(!is.null(inverse)) { ## not null, return retrieved value

        message("getting cached inverse")
        return(inverse)
    }

    ## inverse is not yet computed

    message("calculating and storing inverse")
    data <- x$get() 	## store original matrix
    inverse <- solve(data, ...) 	## calculate inverse
    x$setInv(inverse) 	## store inverse for next time
    inverse 	## return inverse

}
