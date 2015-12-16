## The combination of the functions will create a cacheable matrix object 
## and can be used to inverse a square invertible matrix. 
##
## The makeCacheMatrix function is a cache object used for storing 
## and retrieving if available. 
## The cacheSolve will return the cache matrix object 
## or compute the inverse of a matrix from the cache matrix object 
## and cache it. Inadvertently preventing repetitive costly computation 
## when creating the inverse of the matrix.


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  invertedMatrix <- NULL
  
  # Store matrix and clear inverted matrix
  set <- function(y) {
          x <<- y
          invertedMatrix <<- NULL
  }
  
  # Retrieve matrix
  get <- function() x
  
  # Store inverted matrix
  setInvertedMatrix <- function(obj) invertedMatrix <<- obj
  
  # Retrieve inverted matrix
  getInvertedMatrix <- function() invertedMatrix
  
  # Return list object
  return (
          list(
               set = set
             , get = get
             , setInvertedMatrix = setInvertedMatrix
             , getInvertedMatrix = getInvertedMatrix
              )    
          )

}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  # Attempt to get Inverted Matrix
  InvMat <- x$getInvertedMatrix()
  
  # Check if cache object exists
  if(!is.null(InvMat)) {
          message("Return cached matrix")
          return(InvMat)
  }
  
  #Get matrix from cache object
  objMatrix <- x$get()
  
  # matrix inversion
  InvMat <- solve(objMatrix)
  
  # Set cache
  x$setInvertedMatrix(InvMat)
  
  ## Return a matrix that is the inverse of 'x'
  return(InvMat)
  
}
