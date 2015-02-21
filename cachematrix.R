## This file consists of two functions: makeCacheMatrix & cacheSolve
## makeCacheMatrix functions creates an environment for values to be cached 
## so that they can be accessed via other environments. This is helpful in storing
## results from time expensive oeprations.  

## Function: makeCacheMatrix
## Preconditions: Input Matrix assumed to be 'invertible'
## Postconditions: Getters/Setters to matrix/it's inverted form that can be accessed 
## 		  via other other environments
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


## Function: cacheSolve 
## Preconditions: makeCacheMatrix handler. Defaulted to 1x1, but can be set using makeCacheMatrix's setMatrix.
## Postconditions: Inverted form of the matrix. Not cached for first call. Cached for subsequent calls. 
## 		  via other other environments
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
