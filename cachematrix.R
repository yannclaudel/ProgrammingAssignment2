## Cache system for solve matrix calculation


## makeCacheMatrix is a list of functions that encapsulate the matrix and its inverse
## input : a matrix
## output : list of function

makeCacheMatrix <- function(x = matrix()) {
	  ## inv is the inverse matrix
	  inv <- NULL
	  ## setter
      set <- function(y) {
                x <<- y
                inv  <<- NULL
        }
	 ## getter
     get <- function() x
	 ## setter inverse - put in cache
     setinverse <- function(inverse) inv  <<- inverse
	 ## getter inverse - get from cache
     getinverse <- function() inv 	 
	 ##  return the list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve return the inverse of a matrix, this return value is cache
## input : a makeCacheMatrix
## output : a matrix

cacheSolve <- function(x, ...) {
		## get inverse matrix from cache
        inv <- x$getinverse()
		## if inverse matrix from cache is not null, return it.
        if(!is.null(inv)) {
                    return(inv)
        }
		## if inverse matrix from cache is null, solve it.
        inv  <- solve(x$get())
		## put it in cache.
        x$setinverse(inv)
		## return inverse matrix
        inv
}
