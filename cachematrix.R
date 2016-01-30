## Put comments here that give an overall description of what your
## functions do


## Caching the Inverse of a Matrix:
# Its puspose is to store a martix and a cached value of the inverse of the 

# matrix. Contains the following functions:
# * set      set the value of a matrix
# * get      get the value of a matrix
# * setInverse     get the cahced value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
	   
	   # holds the cached value or NULL if nothing is cached
	  # initially nothing is cached so set it to NULL
        inv <- NULL

		# Store a matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

		# Returns the stored matrix
        get <- function() x

		# Cache the given argument
        setInverse <- function(inverse) inv <<- inverse

		# Get the Cached value
        getInverse <- function() inv

		# returns a list. Each named element of the list is a function
        list(set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	
        # get the cached value
        inv <- x$getInverse()

		# if a cached value exists, pls return it
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

		# otherwise get the matrix, caclulate the inverse and store it in
	  # the cache
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)

		# Return the inverse
		inv
}
