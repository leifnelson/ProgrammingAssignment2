## These functions are very similar to those provided
## in the example but instead of utilizing a vector 
## they utilize a matrix and instead of calculating 
## and caching the mean of the vector, they calculate
## and cache the inverse of the matrix.

## This function creates a "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
			set <- function(y) {
					x <<- y
					inv <<- NULL
			}
			get <- function() x
			setinv <- function(solve) inv <<- solve
			getinv <- function() inv
			list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function creates the inverse of the "matrix" 
## produced by makeCacheMatrix; if the matrix has not
## changed and the inverse has already been calculated,
## then the function retrieves the inverse from the
## cache.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
	}
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
