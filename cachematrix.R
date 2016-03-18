## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This is a pair of function that cache the inverse of a matrix.

## The first function right below is a special "matrix" object that cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(get=get, set=set, setinverse=setinverse, getinverse=getinverse)
}


## The second function right below is a function to compute the special matrix created by makeCacheMatrix.
## It first check if there already has been calculated the inverse.
## If so, it gets the inverse from the cache and skip the computation.
## Otherwise, it calcuates the inverse of the matrix and sets the value of the inverse in the cache with setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m,...)
  x$setinverse(inv)
  inv
}


## Test running
> test_matrix<- makeCacheMatrix(matrix(1:4,2,2))
> test_matrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> test_matrix$getinverse()
NULL
> cacheSolve(test_matrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(test_matrix)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> test_matrix$getinverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
