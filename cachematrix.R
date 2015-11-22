## Following is a pair of functions that cache the inverse of a matrix: 
## 1. makeCacheMatrix() - makes a special matrix object that can cache its inverse 
## 2. cacheSolve() - computes the inverse of the special "matrix" returned by makeCacheMatrix or returns cached inverse

##makeCacheMatrix: 
##This function creates a special "matrix" object that can cache its inverse,
## which is really a list containing a function to:
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse matrix
#4. get the value of the inverse matrix
## Assume that the matrix supplied is always invertible
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#cacheSolve: 
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve
# the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
