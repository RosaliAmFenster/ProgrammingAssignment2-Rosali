#this is a program for caching the invert of a matrix to save computing time, updated version

makeCacheMatrix <- function(my_matrix = matrix()) {
  #this is the first part, the function saves the cache of the matrix
  
  m <- NULL
  
  set <- function(y) {
    my_matrix <<- y;
    m <<- NULL;
  }
  
  get <- function() my_matrix;
  setinv <- function(solve) m <<- solve;
  getinv <- function() m;
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

cacheSolve <- function(my_matrix = matrix(), ...) {
  #this is the second part, the function returns the invert of the given matrix
  
  my_inverse_matrix <- my_matrix$getinv()
  if(!is.null(my_inverse_matrix)) {
    message("Getting cached data...")
    return(my_inverse_matrix)
  }
  data <- my_matrix$get()
  my_inverse_matrix <- solve(data, ...)
  my_matrix$setinv(my_inverse_matrix)
  return(my_inverse_matrix)
}