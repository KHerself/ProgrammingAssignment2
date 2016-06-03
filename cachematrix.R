## This program caches the inverse of a matrix

## This function computes the inverse of a matrix and stores it in variable x

makeCacheMatrix <- function(x = matrix()) {
	xi <- NULL
	set <- function(y) {
		x <<- y
		xi <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) xi <<- solve
	getinverse <- function() xi
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function determines if a cached version of the inverse of the matrix already exists, and if so, pulls it without recomputing


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xi <- x$getinverse()
    if(!is.null(xi)) {
    	message("getting cached data")
    	return(xi)
    }
    data <- x$get()
    xi <- solve(data, ...)
    x$setinverse(xi)
    xi
}
