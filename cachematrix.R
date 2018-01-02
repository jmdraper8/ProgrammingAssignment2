## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    # Initialize the inverse property
    m <- NULL
    
    ## Method to set the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Method the get the matrix and return the matrix
    get <- function() x
    
    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) m <<- inverse
    
    ## Method to get the inverse of the matrix and return the inverse property
    getInverse <- function() m
    
    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## Return the inverse if its already set and print a message
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Get the matrix from the object
    data <- x$get()
    
    ## Calculate the inverse using the matr5ix funciton solve
    m <- solve(data, ...)
    
    ## Set the inverse to the object
    x$setInverse(m)
    
    ## Return the matrix
    m
    
}
