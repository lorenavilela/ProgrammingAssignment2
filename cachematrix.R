
## Function receives a matrix as a parameter and set its value in cache
## It returns a list of functions (get,set, setSolve, getSolve) of the matrix
makeCacheMatrix <- function(x = matrix()) {
    
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


## Function inverses the matrix created with the function makeCacheMatrix
## Checks if the matrix has already been inversed before
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        
        mat <- x$getSolve()
        
        ##Verify if the matrix has been inversed before
        if(!is.null(mat)) {
            message("getting cached data")
            return(mat)
        }
        
        data <- x$get()
        
        ## solve() is used to inverse the matrix
        mat <- solve(data, ...)
        
        x$setSolve(mat)
        
        mat
}


