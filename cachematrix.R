## Developed by Praveen Darshanam
## Put comments here that give an overall description of what your
## functions do

##  makeCacheMatrix creates a list containing functions to 
##  set the values of the matrix
##  get the values of the matrix
##  set the value of the inverse of matrix
##  get the value of the inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
	## setting variable which holds inverse matrix to NULL
	invofmat <- NULL
	set <- function(y) {
		## assigning values to objects from different environment
		x <<- y
		invofmat <<- NULL
	}
	## get matrix
	get <- function() x
	setinv <- function(solve) invofmat <<- solve
	getinv <- function() invofmat
	list(set = set, get = get,
	     setinv = setinv, getinv = getinv)
}


## computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        print("class of x")
        print(class(x))
        invofmat <- x$getinv()
        if(!is.null(invofmat)) {
        	message("getting cached data")
        	return(invofmat)
        }
        mdata <- x$get()
        if (nrow(mdata) != ncol(data)) {
        	print("Non-square matrices do not have inverses")
        	## return from here, can't calculate inverse
        }
        ## calculate inverse of a matrix
        invofmat <- solve(mdata, ...)
        ## setting/caching inverse of a matrix variable 
        x$setinv(invofmat)
        invofmat
}
