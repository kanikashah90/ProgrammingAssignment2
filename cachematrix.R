## This function will calculate the inverse of a matrix in a efficient way 
#by caching the results

## This function provides helper function for caching matrix inverse
# setMatrix: stores the given matrix in variable x and reintialize inverse to null
# getMatrix: returns the matrix x
# getInverse: returns the inverse of a matrix currently stored in variable x
# setInverse: caches the inverse of matrix x in variable invert

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  getMatrix <- function() x
  setMatrix <- function(y) {
    x <<- y
    invert <<- NULL
  }
  setInverse <- function(inverse) invert <<- inverse
  getInverse <- function() invert
  list(getMatrix = getMatrix, setMatrix = setMatrix, setInverse = setInverse, getInverse = getInverse)
}


## This function calculates the inverse of a matrix following these steps
#1. Find if the cache exists or not
#2. If answer to step 1 is yes, return the cache value
#3. If answer to step 1 is no, calculate the inverse and save it in cache

cacheSolve <- function(x, ...) {
  invert <- x$getInverse()
  if (!is.null(invert)) {
    return (invert)
  }
  matrix <- x$getMatrix()
  invert <- solve(matrix)
  x$setInverse(invert)
  invert
}
