## Put comments here that give an overall description of what your
## functions do

## This funtion creates an object that holds a matrix
## and associated functions, namely
## get() to return the matrix, set() to assign a 
## new set of values to the matrix. getinverse() to 
## return the inverse of the matrix, and setinverse()
## to assign a new value to inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks if an inverse of the matrix is cached
## If yes, it returns the cached value
## If not, the inverse is calculated with the solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
