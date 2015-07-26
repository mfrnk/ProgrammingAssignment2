## The solution of Programming Asssignment 2 
## consists of two functions, one to create 
## a "matrix with cacheable inverse" abstraction
## and one to invert the matrix, or retrieve the 
## cached inverse, if it was already calculated 
## before

## This function creates a matrix abstraction
## in order to cache the inverse of the matrix.
## The matrix abstraction consists of a list
## containing functions to set and get the matrix
## and set and get its inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
		}
	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list(set = set, get = get,
	  setinverse = setinverse,
	  getinverse = getinverse)
}

## This function returns the inverse of the matrix, 
## either calling solve to calculate it, or returning
## the cached value, if it was calculated before
cacheSolve <- function(x, ...) {
		inv <- x$getinverse()
		if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
		}
		inv <- solve(x$get(), ...)
		x$setinverse(inv)
		inv
}
