## Function creates matrix object that can cache its inverse, including setting and getting the intial matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL

        }
        get <- function() x
        setinverse <- function(inverse) inverse <<- inverse
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function outputs inverse of initial matrix returned by makeCacheMatrix. Function will retrieve the cache of the inverse if the inverse has already been computed.

cacheSolve <- function(x, ...) {

        inverse <- x$getinverse()
        if(!is.null(inv)) {
                return(inverse)

        }

        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}
