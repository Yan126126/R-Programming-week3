# Define a function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(newx) {
    x <<- newx
    inv <<- NULL # Clear the cached inverse when the matrix is changed
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)
}

# Define a function to compute and cache the inverse of the special "matrix"
cacheSolve <- function(x, ...) {
  # First check if the inverse has already been computed
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) # Return the cached inverse if it exists
  }
  
  # If no cached inverse exists or the matrix has changed, compute it
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the newly computed inverse
  x$setInverse(inv)
  inv # Return the computed inverse
}