
##This function creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) I <<- Inv
        getInv <- function() I
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
         I <- x$getInv()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setInv(I)
        I
}
