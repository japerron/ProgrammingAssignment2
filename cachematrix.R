## Antonio Peralta
## Programming Assignment 2
## R Programming, rprog-013
##
## These functions return the inverse of an input matrix. The inverse can be 
## retrieved from cache if the input matrix is the same as the one from the 
## previous execution.

## makeCacheMatrix caches a matrix and its inverse and provides 
## access to them via get and set methods. The function is a contextualized 
## copy of the makeVector example for the assignment.

makeCacheMatrix <- function(x = matrix()) {
     ## x: invertible matrix
     inv <- NULL
     set <- function(y) {
          ## Note the <<- operator for parent-environment assignements
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(inverse) inv <<- inverse
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## cacheSolve works with makeCacheMatrix to return the inverse of an input
## matrix. If the matrix is uncached, it calculates its inverse and uses 
## makeCacheMatrix to store them. If the matrix is cached, it returns the 
## cached inverse. The function is a contextualized copy of the setMean 
## example for the assignment.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getinv()
     ## Is there a cached inverse for this matrix?
     if(!is.null(inv)) {
          ## This is the case where the inverse exists in cache
          message("getting cached data")
          return(inv)
     }
     ## This is the case where the inverse does not exist in cache
     data <- x$get()
     message("calculating inverse and caching")
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}
