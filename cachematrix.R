## This function creates a special "matrix" object that can cache its inverse
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invr <<- inverse
  getInverse <- function() invr
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invr <- x$getInverse()
  ##if the inverse has already been calculated
  if (!is.null(invr)) {
    message("getting cached data")
    return(invr)
  }
  matx <- x$get()
  invr <- solve(matx, ...)
  x$setInverse(invr)
  invr
  
}
