## to create a vector containing Matrix in cache

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL 
        #set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        } 
        # get the value of the matrix
        get <- function() x 
        
        # set the inverse matrix
        setsolve <- function(solve) m <<- solve
        
        # get the value of the inverse matrix
        getsolve <- function() m
        
        
        list(a = set, b = get,
             c = setsolve,
             d = getsolve)
}


## To set the inverse matrix and get the value from cache is any.

cacheSolve <- function(x, ...) {
        m <- x$d()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$b()
        m <- solve(data, ...)
        x$c(m)
        m
        ## Return a matrix that is the inverse of 'x'.
}