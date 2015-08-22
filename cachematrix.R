## These function make it possible to cache the inverse of matrices

## This function creates a special matrix object that can cache iets inverse
## When the matrix is set (anew), the cached inverse is dropped

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	cached_x <- NULL
	set <- function (y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setsolve <- function(inverse) {
		i <<- inverse
	}
	getsolve <- function() i
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" return by makeCacheMatrix above.
## If the inverse has already been calculated, then it retrieved from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	i <- x$getsolve()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setsolve(i)
	
	i
}
