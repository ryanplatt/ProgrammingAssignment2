## These functions provide an optimization for situations where the 
## inverse of the same matrix is repeatedly required.

## The structure is based on the code provided for mean calculation, 
## as provided in the Coursera rprog-015 course. 


## makeCacheMatrix provides an interface to access the optimised matrix
## functions, containing the following list of functions:
##   set(matrix)          -- set the matrix
##   get(matrix)          -- get the matrix
##   getinverse(matrix)   -- return the inverse matrix
##   setinverse(matrix)   -- set the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Checks if the inverse of x has been previously calculated,
## and if so return the previous calculcation. If not, calculate 
## and return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
