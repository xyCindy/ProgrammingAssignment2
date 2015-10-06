## R Programming
## Week 3, Programming Assignment 2

# makeCacheMatrix: 
# This function creates a special "matrix" object that can cache its inverse.
# This function is really a list containing a function to
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the mean
# 4.get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
   # initialize the matrix's inverse to NULL
   inv <- NULL
   
   # set the value of the matrix
   set <- function(y) {
           x <<- y
           inv <<- NULL
   }
   
   # get the value of the matrix
   get <- function() x
   
   # set the value of the inverse
   setinverse <- function(inverse) inv <<- inverse
   
   # get the value of the inverse
   getinverse <- function() inv
   
   # return a list of the above functions
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


# cacheSolve: 
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse 
# from the cache.
cacheSolve <- function(x, ...) {
        # check if the inverse is already cached,
        # if so, we get the inverse from the cache directly
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # else, we first get the matrix
        data <- x$get()
        # then calculate the inverse using solve function
        inv <- solve(data, ...)
        # then set the value of the matrix's inverse
        x$setinverse(inv)
        # return the result
        inv
}