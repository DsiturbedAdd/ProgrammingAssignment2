# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
# Although the 'set' function is not required in the description of the task
#there can be found a phrase in the description of cacheSolve function
#"(and the matrix has not changed)" implying that the matrix can be changed.

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inv_2)
    inv <<- inv_2
    getinverse <- function()
    inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by
#makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
