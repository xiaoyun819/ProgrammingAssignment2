## The following R function is used to cache the inverse of a matrix, 
## so that when we need it again, it can be looked up in the cache, 
## rather that recomputed it.

## The first function, makeCacheMatrix creates a special "matrix", which 
## is really a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(X = matrix()) {
        M <- NULL
        set <- function(Y) {
                X <<- Y
                M <<- NULL
        }
        get <- function() X
        setSolve <- function(solve) M <<- solve
        getSolve <- function() M
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the inverse of the matrix has already been computed. If so, it 
## gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value
## of the mean in the cache via the setmean function.

cacheSolve <- function(X, ...) {
        M <- X$getSolve()
        if(!is.null(M)) {
                message("getting cached data")
                return(M)
        }
        data <- X$get()
        M <- solve(data, ...)
        X$setSolve(M)
        M
}


