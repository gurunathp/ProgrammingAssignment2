## author: Gurunath
## makeCacheMatrix: Data wrapper of matrix to hold the data - matrix and its inversion
## cacheSolve: Provides cache logic using makeCacheMatrix

## Input is a matrix.It provide the wrapping functions setting matrix, getting
## matrix, setting inversion of matrix, getting inversion of matrix.

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


## Input is Cache Matrix.Verifies Inverse of matrix availabe in the input. 
## If availble, it will return as output. Otherwise, calculates inverse of
## matrix and populate in the input and return the inversion matrix as output  

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
