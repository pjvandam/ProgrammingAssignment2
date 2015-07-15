## Some computations in R are potentially time-consuming, 
## e.g. the computation of the inverse of a given matrix. 
## These functions are used to cache the inverse of a matrix.
## A full example can be found at the bottom of the page.

## This function creates a special object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      
      ### set value of matrix
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      ### get the value of the matrix
      get <- function() x
      
      ### set the value of the inverse of the matrix
      setsolve <- function(solve) inv <<- solve
      
      ### get the value of the inverse of the matrix
      getsolve <- function() inv
      
      # return the list with the functions
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}

## The following function computes the inverse of the matrix
## and uses the object created by the makeCacheMatrix object.
## If the inverse has already been calculated 
## then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
      # get from cache
      inv <- x$getsolve() 
      
      # check if inverse is already cached
      # if this is the case, print cached matrix
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      # if cache does not exist
      
      # get matrix
      data <- x$get()
      
      # compute inverse
      inv <- solve(data, ...)
      
      # cache inverse
      x$setsolve(inv)	     
      
      # print inverse
      inv		      
}

## Full example:
## 1. Generate random matrix: testmatrix <- matrix(rexp(100, rate=.1), ncol=10)
## 2. Generate makeCacheMatrix object: x <- makeCacheMatrix(testmatrix)
## 3. Compute inverse, which is not cached yet: cacheSolve(x) 
## 4. Next time, inverse will be pulled from cache: cacheSolve(x) 