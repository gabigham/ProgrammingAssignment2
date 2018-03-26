## these functions create and acess a data type 
## that includes a matrix and its inverse

## makeCacheMatrix takes a matrix x and 
## has getter and setter functions for x and its 
## inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(x) inv <<- solve(x)
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns a cached matrix object's
## inverse and sets it if necessary.

cacheSolve <- function(x) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    x$setinverse(data)
    x$getinverse()
}

