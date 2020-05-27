## Following 2 functions solve and cache the inverse of a matrix and return this inverse matrix when prompted in order to
## save computational time having to repeatedly solve the inverse to the same matrix repeatedly. 

## The makeCacheMatrix function creates a matrix object that is cached.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i<<- inverse 
  getinverse <- function() i
  list(set = set, 
       get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve solves the inverse of the matrix cached above (if it has not already done so) and returns the solution.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
