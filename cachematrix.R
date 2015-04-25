## Programming Assignment week 2
##
## write a pair of functions that cache the inverse of a matrix:
##
## - makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## - cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above.
##
## If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R.
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
##
## For this assignment, assume that the matrix supplied is always invertible.
##


## makeCacheMatrix(x = matrix())
## Defining the setters and getters for the matrix; the setters allocate the variables
## and assign them values; the getters retrieve those values
## When done, return a list of setters and getters.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL				# instantiate inverted matrix variable, empty
	set <- function(z) {
		message('[re]setting matrix; clearing cached data')
		x <<- z
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(invrtd) inv <<- invrtd	# cache 'invrtd' in inv!
	getinv <- function() inv
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)
}

## cacheSolve(lf)
## Given the list of functions 'lf' returned from makeCacheMatrix, find
## the cached inverted matrix result; if it does not exist, calculate it
## and store it in cache.
## NB: according to instructions, we are assuming that the matrix provided
## is always invertible, thus no check / error catching is implemented

cacheSolve <- function(lf, ...) {
	inv <- lf$getinv()
	if(!is.null(inv)) {	# is 'inv' not null / is there cached data?
		message("data found in cache")
		return(inv)		# return means code will exit the function here
	}
	x <- lf$get()
	invrtd <- solve(x)
	lf$setinv(invrtd)
	invrtd
}