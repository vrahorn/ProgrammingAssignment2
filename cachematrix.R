## This R file contains a pair of functions that cache the inverse of a matrix
## Assumption: The matrix supplied is always invertible.
## Note: makeCacheMatrix must be called prior to cacheSolve.

## The makeCacheMatrix creates a special matrix object that can cache its 
## inverse. The function will:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## 1. Set the value of the matrix
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## 2. Get the value of the matrix
        get <- function() x
        
        ## 3. Set the inverse of the matrix
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        
        ## 4. Get the inverse of the matrix
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix is not changed), then cacheSolve should retrieve the inverse
## from the cache. The cacheSolve function will:
## 1. Get the inverse of the matrix
## 2. Check to see if an inverted matrix is already calculated
## 3. If not, get the inverse of the matrix
## 4. Set the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## 1. Get the inverse of the matrix
        i <- x$getinverse()
        
        ## 2. Check to see if an inverted matrix is already calculated
        if (!is.null(i)) {
                print("getting cached data")
                return(i)
        }
        ## 3. If not, get the inverse of the matrix
        data <- x$get()
        i <- solve(data, ...)
        
        ## 4. Set the inverse of the matrix
        x$setinverse(i)
        i
}