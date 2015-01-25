## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## Return the list of get and set functions
    
        m <- NULL
        
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        
        get <- function() x
        
        setSolve <- function(solve) m <<- solve
        
        getSolve <- function()
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        
        mat <- x$getSolve()
        
        if(!is.null(mat)) {
            message("getting cached data")
            return(mat)
        }
        
        data <- x$get()
        
        mat <- solve(data, ...)
        
        x$setSolve(mat)
        
        mat
}


