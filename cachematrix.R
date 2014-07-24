## makeCacheMatrix and cacheSolve cache the invert matrix of a matrix thus avoid 
## repeated computation of its invert matrix.

## makeCacheMatrix create a special matrix, which is a list containing functions 
## to set/get the value of the matrix and its invert matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinvert <- function(inv) i <<- inv
    getinvert <- function() i
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)

}


## cacheSolve gets the invert matrix of CacheMatrix object x. It first checks
## if the invert matrix of x has been calculated before. If so, the cached value 
## will returned. If not, the inverted matrix is calculated using solve 
## function from base package, cached in x through setinvert function and then 
## returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinvert()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinvert(i)
    i
}
