# The first function, makeCacheMatrix(), creates an R object which stores a matrix 
# and its inverse. 
# The second function, cacheSolve(), either calculates and outputs the inverse 
# of the input matrix of the first function, or if cached previously, 
# returns the inverted matrix, as calculated previously, 
# from the makeCacheMatrix() object's environment. 

# makeCacheMatrix() builds a set of functions and return the functions within a list
# to the parent environment. It contains two data objects (x and z) and four functions
# (get(), set(), getsolve() and setsolve()). 

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function (y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setsolve <- function(matrixinverse) z <<- matrixinverse 
  getsolve <- function() z
  list (set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

# cacheSolve() needs an input argument of type makeCacheMatrix() to return a matrix that 
# is the inverse of "x". 

cacheSolve <- function(x, ...) {
  z <- x$getsolve()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setsolve(z)
  z
}
