## The makeCacheMatrix funtion creates the special matrix
## then sets the value of the matrix, gets the value of the matrix, 
## sets the inverse of the matrix, and gets the inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function checks if the inverse of the special matrix exists.
#If the inverse exists, it gets the inverse from cache
#If it does not exist, it calculates the inverse using the solve function and sets the 
#value of the inverse into cache using the setinverse function. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}