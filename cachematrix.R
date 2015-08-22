## The functions are to get the inverse from a matrix by caching first and skip
## the computation. If we don't have the calaulated result in cache then they 
## calculate the inverse matrix instead.

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv = NULL
	set = function(y) {
		x <<- y	
		inv <<- NULL
	}
	get = function() x
	setinverse = function(inverse) inv <<- inverse
	getinverse = function() inv
	list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## This function computes the inverse of the matrix returned by 
## the function above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}