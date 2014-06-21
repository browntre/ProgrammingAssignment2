## These functions compute (if necessary) then cache the inverse of a matrix

## makeCacheMatrix(): creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    
    ## Function to set value of matrix 'x'
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    ## Function to retrieve matrix 'x'
    get <- function() x
    
    ## Function to set/cache inverse of matrix 'x'
    setinverse <- function(solve) m <<- solve
    
    ## Function to retrieve cached inverse of matrix 'x'
    getinverse <- function() m
    
    ## Return list of matrix object functions
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
    
}


## cacheSolve(): computes the inverse of the "matrix" returned by 
## makeCacheMatrix() above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve() retrives the 
## inverse from the cache

cacheSolve <- function(x, ...) {
  
    ## Retrieve cached inverse of 'x', if it exists
    m <- x$getinverse()
    if (!is.null(m)){
        message("Getting cached data")
        return(m)
    }
    
    ## Retrieve matrix 'x'
    data <- x$get()
    
    ## Calculate and set/cache inverse of 'x'
    m <- solve(data)
    x$setinverse(m)
    
    ## Return inverse of 'x'
    m

  
  
}
