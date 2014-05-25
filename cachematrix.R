## makeCacheMatrix - getter setter functions for the getting and setting Matrix
## and Inverse Matrix
## 
## 

makeCacheMatrix <- function(mat = matrix()) {
  invMat <- NULL
  
  set <- function(y) { # Sets a new Matrix
    mat <<- y
    invMat <<- NULL
  }
  
  # returns  Matrix
  get <- function() { mat }
  
  # sets   Matrix Inverse
  setMatrixInverse <- function (matInverse) { invMat <<- matInverse }
  
  
  # returns  Matrix Inverse
  getMatrixInverse <- function() { invMat }
  
  list(set = set, get = get, setMatrixInverse = setMatrixInverse, getMatrixInverse = getMatrixInverse)
  
}


## cacheSolve - Computes, caches, and returns new matrix inverse 

cacheSolve <- function(x, ...) {
  invMat <- x$getMatrixInverse()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat) ## Return cached Matrix
  }
  ## get the matrix and calculate its Inverse
  data <- x$get() 
  invMat <- solve(data) 
  x$setMatrixInverse(invMat)
  invMat       ## Return a matrix that is the inverse of 'x'
}
