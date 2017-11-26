## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a list containing a function to
# 1. set the value of matrix
# 2. get the value of matrix
# 3. set the value of inverse of matrix
# 4. get the value of inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}


## Write a short comment describing this function
# cacheSolve is a function that returns the inverse of matrix. It is assumed that the matrix is always invertible.
# It solves the inverse and caches it using setinverse function if not computed before else it retrieves the inverse from the cached data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Inverse already computed, retrieving cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
