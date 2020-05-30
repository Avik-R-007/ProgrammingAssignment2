## Put comments here that give an overall description of what your
## functions do

## The following function takes a matrix as input and
## set a matrix and get a matrix
## set matrix inverse and get matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) matrix_inverse <<- inv
  getinverse <- function() matrix_inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverse value in the cache
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix_inverse <- x$getinverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  data <- x$get()
  matrix_inverse <- solve(data, ...)
  x$setinverse(matrix_inverse)
  matrix_inverse
}
