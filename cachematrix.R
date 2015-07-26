## makeCacheMatrix(x):
## This function creates a special "matrix" object that can cache its inverse.

### Version 0.2 (2015-07-26 PM; E. Chen)
###   
### Version 0.1 (2015-07-26 AM; E. Chen):
###   Intro to R Programming Assignment #2: pseudocode for HW


makeCacheMatrix <- function(x = matrix()) {

## 1: Set the value of the matrix
  InvM <- NULL
  set <- function(y) {
    x <<- y
    InvM <<- NULL
  }
## 2: Get the value of the matrix
  get <- function() x

## 3: Set the value of the inverse matrix
  setInv <- function(solve) InvM <<- solve

## 4: Get the value of the inverse matrix
  getInv <- function() InvM
  list(set = set, get = get,
       setInv = setInv, getInv = getInv)  

}


## cacheSolve(x)
##   This function computes the inverse of the special matrix
## returned by makeCacheMatrix().
##   If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache 

cacheSolve <- function(x) {
## Return a matrix that is the inverse of 'x'

##   Single-argument version of solve() [solve(arg1)] 
## returns the inverse of arg1 (arg2 when NULL is set to identity matrix) 

  InvM <- x$getInv()
  if(!is.null(InvM)) {
## Get cached inverse InvM if it exists
    message("getting cached data")
    return(InvM)
  }
##   If no cached InvM, calculate the inverse 
## and set it in the cache using setInv()
  data <- x$get()
  InvM <- solve(data)
  x$setInv(InvM)
  InvM  
}
