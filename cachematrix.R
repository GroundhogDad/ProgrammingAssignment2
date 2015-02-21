## cachematrix.R
##
## This file consists of 2 functions
##
## makeCacheMatrix: which creates a special "matrix" object that can cache its inverse.
##
## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.
##
## Computing the inverse of a square matrix is done using the solve function. 
## In this this code it is, assumed that the matrix supplied is always invertible. 
##

## makeCacheMatrix: Creates a special "matrix" object (a list) that can cache the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
	
        inv <- NULL
        
        ## set function:
        ## set the value of the matrix
        set <- function(y) {
        	## if set to a new value the cached inverse is no longer valid
        	if (!all(x==y)) {
                	x <<- y
                	inv <<- NULL
                }
        }
        
        ## return the value of the matrix
        get <- function() x
        
        ## set the cached value of the inverse 
        setInverse <- function(inverse) inv <<- inverse
        
        ## return the value of the inverse matrix
        getInverse <- function() inv 
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve: compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse 
## from the cache.
## Computing the inverse of a square matrix can be done with the solve function.
## Note: if any errors are thrown by the solve() function they will currently cause the program to fail.

cacheSolve <- function(x, ...) {

	## sanity clause to avoid cryptic error if the wrong type of object is passed in
	 if ((!(class(x) == "list"))||(is.null(x$getInverse)) ||(is.null(x$getInverse)))
	 	stop("Input was not a cacheMatrix. Expecting something created using the  makeCacheMatrix(x,...) method, where x is a matrix ")

        inv <- x$getInverse()
        
        ## check both that inverse is set and that the values  haven't changed since the 
        ## inverse was last calculated
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        
        ## use solve to calculate the inverse of the matrix
        ## Note: for the purposes of this program it is assumed the matrix passed is invertible
        ## (could put a try/catch here to catch any exceptions thrown by solve)
        inv <- solve(data, ...)
        
        ## cache the inverse by using the setInverse 
        x$setInverse(inv)
        
        ## return the inverse
        inv
}

