## Programming Assignment, caching the Inverse of a Matrix

## Takes a matrix and creates a special object that caches the inverse mean of
## a matrix.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL

        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) im <<- inverse
        getinverse <- function() im

        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Compute the inverse matrix of the object returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data)
        x$setinverse(im)
        im
}
