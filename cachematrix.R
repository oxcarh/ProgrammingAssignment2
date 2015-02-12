## Author Oscar A. Hernandez

## Creates a special matrix than can store in cache the inverse of the same matrix.
makeCacheMatrix <- function(x = matrix()) {

  cachedInverse <- NULL
  
  ## Sets the matrix 
  setMatrix <- function(matrix) {
    x <<- matrix
    ## When matrix is reasigned, we lose the cached Inverse
    cachedInverse <<- NULL
  }
  
  ## Gets the matrix 
  getMatrix <- function() {
    x
  }
  
  ## Sets the inverse of the Matrix 
  setInverse <- function(inverse) {
    ## Stores it in the cache
    cachedInverse <<- inverse
  }
  
  ## Gets the cached inverse of the Matrix
  getInverse <- function() {
    cachedInverse
  }
  
  list(
    setMatrix = setMatrix, 
    getMatrix = getMatrix, 
    setInverse = setInverse, 
    getInverse = getInverse 
  )
  
}


## Calculates the inverse of a Matrix. If returns the cached inverse if there is one.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    ## Returns the cached version
    return(inverse)
  }
  ## When there is no cached inverse of the matrix, calculates it
  matrix <- x$getMatrix()
  inverse <- solve(matrix)
  ## And stores the inverse in the cache
  x$setInverse(inverse)
  inverse
}
