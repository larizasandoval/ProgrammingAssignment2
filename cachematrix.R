##With these functions you can create an object that contains a matrix and then calculate 
##its inverse if it is not in cache, while if it is not in the cache, it is recalculated

## this function receives an array, 
##creates an object and returns a list with the object's propertie

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##this function receives an object x created with the function done above, 
##and returns the inverse of the matrix that contains x

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
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