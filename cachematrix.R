## makeCacheMatrix is a function that stores a list of four functions:
## 1) set(y) is a function that changes the matrix x stored in the main function 
## to a new matrix y;
## 2) get() is a function that returns the matrix x stored in the main function;
## 3) setinv() stores the value of the input inv in a variable minv into the main 
## function;
## 4) getinv() returns the value of minv.
makeCacheMatrix <- function(x = matrix()) 
{
  minv <- NULL
  set <- function(y) 
  {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) minv <<- inv
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
## cacheSolve is a function that return a matrix that is the inverse of x. The 
## input of cachesolve is the object where makeCacheMatrix is stored;
## cacheSolve verifies the value of minv that was stored by function getmean in 
## makeCacheMatrix function;
## If minv is equal to NULL, data gets the matrix stored by makeVector, 
## solve(data,...) calculates the inverse of data and stores its value in minv 
## with function setinv;
## If minv is not NULL, means that the inverse matrix of x exists in memory.
## The funcion returns a message (getting cached data) and display the value of 
## minv.
cacheSolve <- function(x, ...) 
{
##  minv <- x$getinv()
##  if(!is.null(minv)) {
##    message("getting cached data")
##    return(minv)
##  }
##  data <- x$get()
##  minv <- solve(data, ...)
##  x$setinv(minv)
##  minv
  minv <- x$getinv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setinv(minv)
  minv
}