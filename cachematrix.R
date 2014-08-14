# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly 
# The following function allows caching of the inverse matrix.

# This function creates a special "matrix" object that can cache its inverse
# The function adds the functionality by giving a list of operations to:
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse
# 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


# The following function calculates the inverse of the special "matrix" 
# created with the above function. 
# It first checks to see if the inverse is cached. 
# If yes, it returns the cached value. 
# Else, it calculates the inverse of the matrix and sets the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
