## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## defines set function that assigns parameter to x, NULL to m
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        ## defines get function that returns x
        get <- function() x
        
        ## defines setinv function that assigns the parameter to m
        setinv <-function(solve){
                m <<- solve
        }
        
        ## defines getinv function that returns m
        getinv <- function() m
        
        ## list with all the functions to be returned
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## evaluates getinv of x and assigns its value to m
        m <- x$getinv()
        
        ## if m is not null, it was previously evaluated and returns m
        if(!is.null(m)){
                message("Getting cached data")
                return(m)
        }
        
        ## if it was not evaluated before, we evaluate it
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        
        ## retrun the inverse matrix
        m
}
