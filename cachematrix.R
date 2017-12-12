## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
		im <- NULL
		set <- function(y) {
				x <<- y
				im <<- NULL
		}
		get <- function() x
		setinverse <- function(inversematrix) im <<- inversematrix
		getinverse <- function() im
		list(set = set, get = get,
			 setinverse = setinverse,
			 getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		cachedinversematrix <- x$getinverse()
        if(!is.null(cachedinversematrix)) {
                message("getting cached inverse matrix")
                return(cachedinversematrix)
        }
        specialmatrix <- x$get()
        inversematrix <- solve(specialmatrix)
        x$setinverse(inversematrix)
        inversematrix
}
