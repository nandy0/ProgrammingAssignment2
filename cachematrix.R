## Overall description of the 2 functions
# The two functions creates the effect of caching.

# Input to makeCacheMatrix should an invertible matrix
# It can set or get a matrix, set or get its inverse
# inv refers to the matrix which will contain the inverse of matrix x

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(Inverse) inv <<- Inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# cacheSolve function returns the existing inverse of the matrix.
# It takes an argument which is an object of makeCacheMatrix - list
# It calaculates the matrix's inverse only when the inverse is not previously 
# calculated i.e. when inv is NULL
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
