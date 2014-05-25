## Programming Assignment 2

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x=matrix()) {
        inverse<-NULL
        set<-function(y) {
                x<<-y
                inverse<<-NULL
        }
        get<-function() x
        set_inverse<-function(inv) inverse <<- inv
        get_inverse<-function() inverse
        list(set=set, get=get, 
             set_inverse=set_inverse,
             get_inverse=get_inverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
## If the inverse has not been calculated, then the cacheSolve should calculates and caches the inverse of the matrix.

cacheSolve<-function(x, ...) {
        inverse<-x$get_inverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data<-x$get()
        inverse<-solve(data, ...)
        x$set_inverse(inverse)
        inverse
}

