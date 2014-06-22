## R Programming 
## Assignment 2
## wlbphysics

## makeCacheMatrix initializes (sets) a square matrix 'x' and its inverse. 
## makeCacheMatrix also retrieves (gets) a square matrix 'x' and its inverse.
makeCacheMatrix <- function(x = matrix(), nrow, ncol) {

 	m <- NULL

        set <- function(y = matrix(),nrow, ncol) {
                x <<- y
                m <<- NULL
        }

        get <- function() matrix(x,nrow,ncol)
		
	 
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## cacheSolve calculates the inverse of a square 'x', saves (caches)
## and returns the result.
## If calcheSolve is called twice or more the saved (cached) result is returned.

cacheSolve <- function(x, ...) {
        

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

###################################### SAMPLE OUTPUT ########################################
#	a <- makeCacheMatrix(c(1,1,2,3),2,2)
#	a$get()
#	     [,1] [,2]
#	[1,]    1    2
#	[2,]    1    3
#
#	> cacheSolve(a)
#	     [,1] [,2]
#	[1,]    3   -2
#	[2,]   -1    1
#	> cacheSolve(a)
#	getting cached data
#	     [,1] [,2]
#	[1,]    3   -2
#	[2,]   -1    1
#
# Source: Example 27, matrix A, Elementary Linear Algebra, 5th Edition, Howard Anton, p.38
#############################################################################################
