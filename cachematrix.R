
## Creates a matrix object with 4 methods (set, setInv, get, getInv)
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL  ## cached version of inverse
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInv <- function(solve) m <<- solve
     getInv <- function() m
     list(set = set, get = get,
          setInv = setInv,
          getInv = getInv)


}


## Inverts the contents of the matrix object
## Uses the cached inverse if it is set
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getInv()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setInv(m)
     m
}
