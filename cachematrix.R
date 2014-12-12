

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	#cached inverse will be stored here
	inverse <- NULL
	## setter function
	set <- function(y) {
    		x <<- y
      	inverse <<- NULL
    	}
	## getter function
	get <- function() x
	## seter function for inverse
      setinverse <- function(inv) inverse <<- inv
      ## getter function for inverse
	getinverse <- function() inverse
	## return the object that holds the matrix and can store its cached inverse
      list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
	  ## retrieve cached inverse
        inverse <- x$getinverse()
	  ## if the inverse has been previously been calculated and cached, return inverse from cache
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
	  ## if inverse has not been previously cached, get the data from the matrix object
        data <- x$get()
	  ## calculate the inverse of the matrix
        inverse <- solve(data, ...)
        ## cache the inverse for potential future use
        x$setinverse(inverse)
        ## return the inverse"
        inverse
}
