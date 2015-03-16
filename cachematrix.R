# Computing inversions of matrixs is a time-consuming operation
# This file defines two functions to speed inversion operation to a certain degree
# The key idea is caching, that's to say, if the matrix has been computed before, 
# then, we can return the inversion of this matrix from cache without calling 'solve' function

# hold the inversion of one matrix and provide four basic functions for 
# setting and getting operation
	
makeCacheMatrix <- function(mat = matrix()) {
	inv <- NULL
	set <- function(m) {
		mat <<- m
		inv <<- NULL
	}
	get <- function() mat
	setInv <- function(i) inv <- i
	getInv <- function() inv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}

# x -- list returned from makeCacheMatrix
# y -- matrix needed to be computed this time

# If the matrix is identical with data of last time, return result from cache
# else compute inversion of this data and update the cache
cacheSolve <- function(x, y, ...) {
	if(!is.null(x$getInv()) & identical(y, x$get())) {
		return(x$getInv())
	} else {
		inv <- solve(y)
		x$setInv(inv)
		return(inv)
	}
}
