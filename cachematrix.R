## Put comments here that give an overall description of what your
## functions do

## This makeCacheMatrix function creates a "matrix" object that can cache its inverse.
## The "matrix" made in this function is a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(Solve) m <<- Solve
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)

}


## This cacheSolve function calculates the inverse of the special "matrix" 
## created by the above function.
## Firstly, it checks if the inverse has already been calculated (and the matrix
## has not changed). If so, the cachesolve retrieves the inverse from the cache.
## If not, it will calculates the inverse of the data and set the value of the 
## inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()               ## Assign the value of x to m
        if(!is.null(m)) {               ## Check if the inverse exists or not              
                message("getting cached data")
                return(m)               ## If the inverse exists, return the 
                                        ## value from cache.
        }
        data <- x$get()                 ## If the inverse has not been calculated,
        m <- solve(data, ...)           ## the solve() function calculates the it,
        x$setSolve(m)                   ## and sets the value of the inverse in the cache.
        m
}
