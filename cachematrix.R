#### Functions for Programming Assignment 2: Lexical Scoping ####

## A pair of functions that cache the inverse of a matrix.
## Assumes the matrix supplied is invertible

# This function creates a special "matrix" object that 
# can cache its inverse. Use with cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      # solve(X) finds inverse for matrix X
      setinverse <- function(solve) inverse <<- solve
      getinverse <- function() inverse
      list(set=set,get=get,
           setinverse=setinverse,
           getinverse=getinverse)
}


# Computes inverse of special matrix returned by makeCacheMatrix. 
# If inverse has been computed then gets inverse from cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinverse()
      if(!is.null(inverse)){
            message('getting cached data')
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}

