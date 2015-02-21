## cachematrix_alternative.R
##
## An alternative solution to the problem using classes
## This file consists of 2 functions
##
## makeCacheMatrix: which creates a special "matrix" object that can cache its inverse.
##
## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.
##
## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square 
## invertible matrix, then solve(X) returns its inverse.
##
## For this code it is, assumed that the matrix supplied is always invertible.


## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
## this object has 2 slots: original will hold a copy of the matix and inverse will hold the precalculated inverse
makeCacheMatrix <- function(x = matrix()) {

	## representation of the 'special' matrix
	cacheMatrix <- setClass("cacheMatrix", slots = c(inverse = "matrix", original = "matrix"), contains = "matrix")


	## if this is already a 'special' matrix it will be a list containing the elements matrix, inverse and checksum
	if (class(x) == "cacheMatrix"){
		print("This is already a cacheMatrix")
		print(x)
	}else{
		newMatrix <- cacheMatrix(x,original=x)
		print(newMatrix)
	}
}

## cacheSolve: compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square 
## invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {

	## sanity check we have been passed a special matrix and not just an ordinary one        
        if (class(x) == "cacheMatrix"){
        	cached <- x
        }else{
        	warning("This is not a cacheMatrix - will convert internally")
        	cached <- makeCacheMatrix(x,...)
        }
        
        ## Return a matrix that is the inverse of 'x' 
        ## if the original hasn't changed then can use the inverse
        
        if ((length(cached@inverse) == length(cached)) && (all(cached == cached@original))){
        	print (cached@inverse)
        }else {
        	## recalculate the inverse using solve()
        	cached@inverse  <- solve(cached)
        	
        	## update the original
        	cached@original <- cached
        	
        	## return the result
        	print (cached@inverse)
        }	
}
