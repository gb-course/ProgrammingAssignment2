makeCacheMatrix <- function(x = matrix()) {
        invX <- NULL
        setMatrix <- function(y) {
                x <<- y
                invX <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inv) invX <<- inv
        getInverse <- function() invX
     	  list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        invX <- x$getInverse()
        if(!is.null(invX)) {
                message("getting cached data")
                return(invX)
        }
        data <- x$getMatrix()
        invX <- solve(data)
        x$setInverse(invX)
        invX
}
