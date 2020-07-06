## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates the object to calculate the matrix inverse 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function:
## solves the matrix inverse if its the first time
## or retrieve the cached inverse matrix if it already has been calculated, 
## also measures the execution time to evaluate the performance improve
cacheSolve <- function(x, ...) {
  start_time <- Sys.time()
  print(start_time)
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached matrix")
    end_time <- Sys.time()
    print(end_time)
    print(end_time - start_time)
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  end_time <- Sys.time()
  print(end_time)
  print(end_time - start_time)
  m
}