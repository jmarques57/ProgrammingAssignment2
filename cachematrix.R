## The following function returns the inverse of a matrix x,
## but it first checks if the inverse matrix has been previously
## calculated and cashed. If it has, it returns the cashed value.
## If it hasn't, it calculates the inverse matrix, and it cashes 
## its value before returning it

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
				x <<- y
				m <<- NULL
		}
		get <- function() x
		setinverse <- function(solve) m <<- solve
		getinverse <- function() m
		list(set=set, get=get,
				setinverse=setinverse,
				getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
        	  message("getting cached data")
            return(m)	
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

# Example 1
m1=rbind(c(1, -1/4), c(-1/4, 1))
m1
i1 <- solve(m1)
i1
m2 <- i1%*%m1
m2

# Example 2
x <- makeCacheMatrix(m1)
cacheSolve(x)
# this last value is identical to m2 above