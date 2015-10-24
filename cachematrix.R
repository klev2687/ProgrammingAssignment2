# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #define set function to set the value of matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #define get function to get the value of the matrix
  get <- function() x
  
  #define setsolve function to set the value of inverse
  setinv <- function(inverse) inv <<- inverse
  
  #define getsolve function to get the value of inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #load inv
  inv <- x$getinv()
  
  #check if inverse has already been calculated
  if(!is.null(inv)) {
    #get inverse from cache and terminate computation
    message("getting cached data")
    return(inv)
  }
  
  #calculate inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  }
