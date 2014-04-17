
##These functions will create a cache of matrix inversions to later retrieval.
##Repeated calculations with be retrieved from the cache rather than recalculated
##saving processing power.


##This function will create a list of functions to set and retrieve the matrices 
##as well as set and retrieve the inverses of the matrices.


makeCacheMatrix <- function(x = matrix()) { ##Initialize function
	m <- NULL ##Establish variable m
	set <- function(y) { ##initialize sub-function to set matrix 
		x <<- y ##search parent environment for variable x and define
		m <<- NULL ## search parent environment for variable and define as NULL 
		}
	get <- function() x ##define function to retrieve matrix 
	setinverse <- function(inverse) m <<- inverse ##define function to set matrix inverse 
	getinverse <- function() m ##define function to retrieve matrix inverse 
	list(set = set, get = get, ##create list of functions
		setinverse = setinverse,
		getinverse = getinverse)
}


##This function checks the cache for previously calculations
##if no previous calculations, calculates and stores inverse of matrix in cache
##If previous calculation exists, retrieves inverse and prints

cacheSolve <- function(x, ...) { ##initialize function
		m <- x$getinverse() ##sets output of function to m
		if(!is.null(m)) { ##states selection criteria
			message("getting cached data") ##message to print if inverse is stored
			return(m) ##returns message
		}
		data <- x$get() ##sets stored value to data
		m <- solve(data, ...) ## defines inverse matrix solution
		x$setinverse(m) ##sets the inverse matrix solution to the cache
		m ##returns the solution
}







