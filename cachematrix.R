## Following functions are designed to calculate inverted matrices
## and avoid unnecessary recalculation by getting caching results

## The first function creates a list of a functions to get and to change 
## values of original and inverted matrices

makeCacheMatrix <- function() {
	minv <<- NULL
	## The function "set" changes the value of original matrix. 
	## It also sets the value of inverted matrix to zero because its value 
	## needs to be recalculated
	set <- function(y) {
		morig <<- y
		minv <<- NULL
	}
	## The function "get" returns a value of original matrix
	get <- function() morig
	## The function "setinv" records given value of inverted matrix into "minv" 
	## variable
	setinv <- function(matrixInversed) minv <<- matrixInversed
	##function "getinv" returns the value of inverted matrix
	getinv <- function() minv
	list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}

## This function checks if the value of inverted matrix was calculated for current value of original matrix. 
## If not, the function calculates the value of inverted matrix.
## If the value of inverted matrix was already calculated, the function returns cached value.

cacheSolve <- function(x) {
        ## Getting the value of inverted matrix and checking if it is null
		m <- x$getinv()
		if(!is.null(m)) {
			## If the inverted matrix already was calculated the function
			## returns the value from cash and stops working
			message("No need to recalculate. Getting cached data")
			return(m)
		}
		## If the inverted matrix wasn't calculated before, the function gets
		## the value of original matrix, calculates the inverted matrix and 
		## saves its value by using "setinv" function.
		data <- x$get()
		m <- solve(data)
		x$setinv(m)
		## The function returns calculated value of inverted matrix
		m
}
