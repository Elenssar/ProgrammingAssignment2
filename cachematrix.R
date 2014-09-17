## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix creates a list of four functions: 
#set and get functions, to get/set the argument data value, in this case an invertible Matrix.
#setinv and getinv, to set or get the inverse to the argument matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
#cacheSolve use the object returned by makeCacheMatrix, and returns the inverse of that 
#matrix. Internally this function checks first if it was calculated before, in which case 
#returns the stored value, if not, the funcion calculates and store the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

