## The following acts as a wrapper for all the setters and getters
## needed to set and get values for both atrix x and its inverse m
## and to do the initial setting of inverse matrix m to NULL

makeCacheMatrix <- function(x = matrix()) {
    # initialize inverse matrix m to NULL
		m <- NULL
		# declare all set and get functions for both matrix x and its inverse m
		set <- function(y) {
				x <<- y
				m <<- NULL
		}
		get <- function() x
		setinverse <- function(solve) m <<- solve
		getinverse <- function() m
		# list setter and getter functions
		list(set=set, get=get,
				setinverse=setinverse,
				getinverse=getinverse)
}


## The following functions return the inverse of a matrix x,
## but it first checks if the inverse matrix has been previously
## calculated and cashed. If it has, it returns the cashed value.
## If it hasn't, it calculates the inverse matrix, and it cashes 
## its value before returning it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## return inverse m if it is not NULL
        if(!is.null(m)) {
        	  message("getting cached data")
            return(m)	
        }
        ## if inverse m is NULL, calculate its value from x,
        ## set the cashed value to it, and return it
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

# Test Script
m1=rbind(c(1, -1/4), c(-1/4, 1))
m1
i1 <- solve(m1)
i1
m2 <- i1%*%m1
m2

x <- makeCacheMatrix(m1)
cacheSolve(x)
# this last value is identical to m2 above