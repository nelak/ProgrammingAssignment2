## Put comments here that give an overall description of what your
## functions do

## Creates a matrix that can caches the results of the inverse function (solve)
makeCacheMatrix <- function(x = matrix()) {
  
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
}


## Returns the inverse of a matrix, the inverse is calculated only once subsequent calls are cached
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

m <- makeCacheMatrix(matrix(sample.int(15, 4, TRUE), 2, 2))
cacheSolve(m)
