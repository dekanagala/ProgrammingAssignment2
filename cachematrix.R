## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function makes a list with a function that can get/set values
## of the matrix and can also get/set the values of the inverse of the
## matrix.
makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  set <- function(b) {
    a <<- b
    inv <<- NULL
  }
  get = function() a
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## This function checks to see if the inverse of the matrix, if it has
## not, it then finds the inverse of the matrix and returns it. 
cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = a$getinv()
  if(!is.null(inv)) {
    message("Getting Cache Data") 
    return(inv)
  }
  mat.data = a$get()
  inv = solve(mat.data, ...) 
  
  a$setinv(inv)
  return(inv)
}
