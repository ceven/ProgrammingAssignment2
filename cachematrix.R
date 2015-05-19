## Put comments here that give an overall description of what your
## functions do

# The two following functions, makeCacheMatrix and cacheSolve, ensure that the inverse of a same matrix is 
# computed at most once. cacheSolve takes as input an element created with makeCacheMatrix and returns 
# the inverse of the given matrix.

## Write a short comment describing this function
# The function makeCacheMatrix takes as parameter a matrix and returns a list of four functions:
# 1) Set the matrix to the given argument, and set the matrix inverse to NULL, 
#    such that its inverse is computed when required (otherwise a wrong inverse 
#    may be returned, such as the inverse of a previous matrix)
# 2) Return the cached matrix
# 3) Set the inverse of the cached matrix
# 4) Get the inverse of the cached matrix
# The matrix parameter is optional, and the empty matrix will be used if no parameter is given.

makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) x_inverse <<- inverse
  getinverse <- function() x_inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# The function cacheSolve takes as input a list of functions created with makeCacheMatrix
# and returns the inverse of the matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(is.null(inverse)){
    inverse <- solve(x$get())
    x$setinverse(inverse)
  }
  return(inverse)
}