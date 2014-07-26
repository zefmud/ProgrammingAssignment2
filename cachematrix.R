## Following functions are designed to calculate inverted matrices
## and avoid recalculation by getting caching results

## The first function creates a list of a functions to get and to change 
## values of original and inverted matrices

makeCacheMatrix <- function() {
	minv <<- NULL
	## The function "set" changes the value of original matrix. 
	## It also sets the value of inverted matrix to zero because its value 
	## needs recalculation
	set <- function(y) {
		morig <<- y
		minv <<- NULL
	}
	## The function "get" gives a value of original matrix
	get <- function() morig
	## The function "setinv" records given value of inverted matrix into "minv" 
	## variable
	setinv <- function(matrixInversed) minv <<- matrixInversed
	##function "getinv" gives the value of inverted matrix
	getinv <- function() minv
	list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}

## This function checks if the value of inverted matrix was calculated for current value of original matrix. 
## If not, the function calculates the value of inverted matrix.
## If the value of inverted matrix was already calculated, the function returns cached value.

cacheSolve <- function(x) {
        m <- x$getinv()
		if(!is.null(m)) {
			message("No need to recalculate. Getting cached data")
			return(m)
		}
		data <- x$get()
		m <- solve(data)
		x$setinv(m)
		m
}
