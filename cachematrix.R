## Function makeCacheMatrix will be used for assigning functions to a given invertible matrix.
## Function cachesolve will be used to create and cache inverse of invertible matrix. 
## Once cached, the function cachesolve will return cached inverted matrix
## Note: It is assumed these two functions together will work with invertible matrix
## Following commands can be used to verify the caching
## 1:	m <- rbind(c(1, -1/4), c(-1/4, 1))
## 2:	cm <- makeCacheMatrix(m)
## 3:	cm$get()
## 4:	cacheSolve(cm)
## 5:	cm$getinverse()
## Results of steps 4 & 5 should match


## This function assigns 4 functions to given matrix. 
## Two for setting and getting the special matrix (x) itself
## Another two for setting and getting the given inverse of the special matrix

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


## This function tries to get the cached inverse of matrix from the function getinverse()
## If the inverse is not cached already, the function calculates the inverse using solve(X) function
## Then it caches the inverse using setinverse() function. Then it returns the newly created inverse matrix.
## Next time for same speacial matrix, the funciton will get already cached inverse matrix again through function getinverse()

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
