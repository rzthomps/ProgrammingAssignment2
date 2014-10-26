## These functions are going to make and/or cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## must make sure m is defined as NULL
  m <- NULL
  set <- function(a) {
    x <<- a
    m <<- NULL
  }
   get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get , setinv = setinv , getinv = getinv)
    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m  <- x$getinv() 
         ##Tests to see if already done and if so returns cached data 
        if (!is.null(m)){ 
              message("getting cached data") 
              return(m) 
             } 
        ##This will occur if the if statement doesn't cause the matrix (m) to return
        ## 
           data  <- m$get() 
           m  <- solve(data, ...) 
        x$setinv(m) 
        m  } 
## Return a matrix that is the inverse of 'x'

