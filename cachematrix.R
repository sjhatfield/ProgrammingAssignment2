## makeCacheMatrix creates an object with an inv variable which stores the 
## inverse if it has been calculated. initially it is set to null as the 
## inverse is unknown. it has functions to get the cached inverse and set the 
## inversse

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

## this function first checks if the inverse is cached in the variable inv
## by looking at the getinverse element of the list. if it is not null then it
## means an inverse is stored and it is returned, after printing a message.
## if there is no inverse stored then the inverse is calculated using the 
## solve function built into R and then stored in the variable

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    X <- x$get()
    inv <- solve(X, ...)
    x$setinverse(inv)
    inv
}
