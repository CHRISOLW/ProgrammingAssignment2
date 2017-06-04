## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a vector of functions  
## set and get will set and get the Original Matrix
## setInvMatrix and getInvMatrix will and get the Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
  
  invGMatrix <- NULL
  set <- function(y) {
    x <<- y
    invGMatrix <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(invLMatrix) invGMatrix <<- invLMatrix
  getInvMatrix <- function() invGMatrix
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)

}


## cacheSolve is the function that will covert the martrix to inverse 
## cacheSolve will also the cache the inversed matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  invGMatrix <- x$getInvMatrix()
  if(!is.null(invGMatrix)) {
    message("getting cached data")
    return(invGMatrix)
  }
  data <- x$get()
  invGMatrix <- solve(data, ...)
  x$setInvMatrix(invGMatrix)
  invGMatrix
  
}
