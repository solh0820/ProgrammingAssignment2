## Put comments here that give an overall description of what your
## functions do
## this function will create a matrix-object that can be used in order to calculate the inverse

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {## We start defining the parameters of the function setting it as matrix
  inver <- NULL                             ## initialize matrix
  set <- function(y) {                      ## this function help us defining the set function to assign 
    x <<- y                            
    inver <<- NULL                          ## reinit, the matrix in case it exits a new one
  }
  getv <- function() x                     ##  get the value of the matrix 
  
  setinver <- function(inversa) inver <<- inversa  ## assigns value inver in upper enviroments
  valinver <- function() inver                     ## gets the value of inver 
  list(set = set, getv = getv, setinver = setinver, valinver = valinver)  ## we stablish this list in order to link functions with the $ operator
    }


## Write a short comment describing this function
## This function calculates the inverse of the matrix in the above function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$valinver()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinver(inver)
  inver
       
}
