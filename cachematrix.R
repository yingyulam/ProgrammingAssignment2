## These functions are used to cache the inverse of a matrix instead of compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.
## It is a list containing a function to:
## 1.Set the matrix; 2. get the matrix; 3.set the inverse of the matrix; 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the function should retrieve the inverse from the cache,
## with a message "getting cached data".

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
