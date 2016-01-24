
## The first function, makeCahceMatrix creates a special "matrix",
##which is really a list containing a function to:

##1 set the value of the matrix
##2 get the value of the matrix
##3 set the value of the inverse matrix
##4 get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  Ainv <- NULL
  set <- function(y) {
    x <<- y
    Ainv <<- NULL
  }
  get <- function() x
  setAinv <- function(Ainv) Ainv <<- Ainv
  getAinv <- function() Ainv
  list(set=set, get=get, setAinv=setAinv, getAinv=getAinv)
}

## The cacheSolve function returns the inverse of the matrix. 
## If the inverse has already been computed it load the result from the cache.
## If the inverse matrix hasn't computed yet the function does the computations
## and returns computed inverse matrix and sets this inverse matrix in the cache.

cacheSolve <- function(x, ...) {
  Ainv <- x$getAinv()
  if(!is.null(Ainv)) {
    message("getting cached data.")
    return(Ainv)
  }
  data <- x$get()
  Ainv <- solve(data)
  x$setAinv(Ainv)
  Ainv
}
