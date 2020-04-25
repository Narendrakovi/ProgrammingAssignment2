## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x=matrix()){
  ## Providing Initial value 
  k <- NULL
  ## Method to set a matrix      
  setMatrix <- function(y){
    x <<- y
    k <<- NULL
  }
  ## Method to get a matrix
  getMatrix <- function() x
  ## Method to set a inverse function to the given matrix
  setInverse <- function(inverse) k <<- inverse
  ## Method to get the Inverse matrix
  getInverse <- function() k
  ## Return the list of methods
  list(setMatrix = setMatrix, getMatrix= getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...){
 ## Initializing Inverse value 
  k <- x$getInverse()
 ## to check whether the value is present in cache data or not and return the message if the value is present in cache
  if(!is.null(k)) {
    message("getting cache data")
    return(k)
  }
  ## To get the matrix from the object
  data <- x$getMatrix()
  ## To calculate the inverse matrix value
  k <- solve(data,...)
  ## to set the Inverse to the object
  x$setInverse(k)
  ## to return the matrix
  k
}
