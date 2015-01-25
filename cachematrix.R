## Programming Assignment 2: Lexical Scoping
## Assignment: Caching the Inverse of a Matrix 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	## set the value of the matrix
    	set <- function(y) {
     	x <<- y
        	inv <<- NULL
    	}
    	## get the value of the matrix
    	get <- function() x
    	## set the value of the inverse of the matrix
    	setinverse <- function(inverse) inv <<- inverse
    	## get the value of the inverse of the matrix
    	getinverse <- function() inv
    	## return list
    	list(set=set, get=get,
    		setinverse=setinverse,
    		getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	  inv <- x$getinverse()
	
	if(!is.null(inv)) {
		message("getting cached data")
         return(inv)
	}
     
	data <- x$get()
	## calculate the inverse of the matrix
    	inv <- solve(data)
    	x$setinverse(inv)
    	inv
}
