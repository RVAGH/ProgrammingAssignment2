## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#the makecacheMatrix creates a matrix and prints 

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL                  #assign i as NULL, will have value of inverse 
  
  set <- function(y) {       #use the set to assign value of matrix and assign value of
    x <<- y                  #i to NULL
    i <<- NULL
  }
  
  get <- function() x        #return value of matrix passed as argument
  
  setinverse <- function(inverse) i <<- inverse       #assign value of inverse 
  getinverse <- function() i                          #get value of i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)                   
    
}


## Write a short comment describing this function
## This function computes the inverse of the"matrix" returned by makeCacheMatrix above and stores/caches it
## If the inverse is already present and is not new, it will return from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data for matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
