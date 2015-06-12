## Programming Assignment week 2

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
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

## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above.
##
## Given the list of functions 'lf' returned from makeCacheMatrix, find
## the cached inverted matrix result; if it does not exist, calculate it
## and store it in cache.
## No check / error catching implemented

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