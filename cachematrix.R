## Overall description of what the functions do:
## set(y) is a function that changes the matrix stored in the main function to a new matrix "y";
## get() is a function that returns the matrix x stored in the main function;
## setinv() stores the value of the input "inv" in a variable "m" into the main function;
## getinv() returns the value of "m";
## Write a short comment describing this function

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

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) 
{
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