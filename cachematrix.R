## The following functions allow a user to quickly get the inverse
## of a matrix if it has previously been calculated by caching
## the inverse after the first time it is computed.

## This function returns getters and setters for both the original
## matrix and its inverse. Setting a new value for the original matrix
## will reset the cache storage for it.
makeCacheMatrix <- function(matrix = matrix()) {
  cachedInverse <- NULL
  setMatrix <- function(y) {
    matrix <<- y
    cachedInverse <<- NULL
  }
  getMatrix <- function() matrix
  setInverse <- function(inverseMatrix) cachedInverse <<- inverseMatrix
  getInverse <- function() cachedInverse
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function takes the output of makeCacheMatrix() as its input
## as well as any other parameters that need to be passed to the
## solve() function in order to compute the matrix inverse. If the inverse
## has been previously computed, the cached value is returned, otherwise,
## the inverse is computed on the spot.
cacheSolve <- function(matrixObj, ...) {
  inverseMatrix <- matrixObj$getInverse()
  if(!is.null(inverseMatrix)) {
    return(inverseMatrix)
  }
  matrix <- matrixObj$getMatrix()
  inverseMatrix <- solve(matrix, ...)
  matrixObj$setInverse(inverseMatrix)
  inverseMatrix
}
