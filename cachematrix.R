## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
invertedMatrix <- NULL
  
  #Getters
  getMatrix <- function() x
  getInverse <- function() invertedMatrix
  
  
  #Setters
  setInvertedMatrix <- function(invertedMtx) invertedMatrix <<- invertedMtx
  setMatrix <- function(y)
  {
    x <<- y
    invertedMatrix <<- NULL
  }
  
  
  #list function accessors
  list(set = setInvertedMatrix, setMatrix = setMatrix, get = getMatrix, getInvertedMatrix = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMtrx <- x$getInvertedMatrix()
	  
	  if (!is.null(invMtrx))
	  {
	    message("Returning inverted matrix from cache")
	    return (invMtrx)
	  }
	  else
	  {
	    message("Inverted matrix not cached. Creating...")
	    data <- x$get()
	    invrMtx <- solve(data)
	    x$set(invrMtx)
	    invrMtx  
  }
}
