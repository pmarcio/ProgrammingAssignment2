## This file has two functions. The first one makeCacheMatrix has four functions 
## to manipulate the matrix and its Solve. The second one computes the inverse of the
## matrix cached on the first one, or if is already computed returns the cached result.

## Receives a matrix and invMatrix and returns their values with commands:
## 1) setMatrix, stores the matrix supplied;
## 2) getMatrix, returns the Matrix stored;
## 2) setSolveMatrix, stores the inverse of matrix supplied with setMatrix;
## 3) getSolveMatrix, returns the inverse of matrix supplied with setMatrix.

makeCacheMatrix <- function(Matrix = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y) {
    Matrix <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() Matrix
  setSolveMatrix <- function(solve) invMatrix <<- solve
  getSolveMatrix <- function() invMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setSolveMatrix = setSolveMatrix, getSolveMatrix = getSolveMatrix)
}


## Calculates the Solve result if it has not been done earlier, if it already exists
## only returns its value informing that to user.

cacheSolve <- function(Matrix, ...) {
  m <- Matrix$getSolveMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- Matrix$getMatrix()
  m <- solve(data, ...)
  Matrix$setSolveMatrix(m)
  m
}
