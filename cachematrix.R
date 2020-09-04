## This R code uses <<- operator to cache a matrix's
## inverse in the environment of the function objects in the list
## representing the matrix

## This function returns a list with function objects
## which are getters and setters for the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  getInverse <- function() inverse
  setInverse <- function(inv) inverse <<- inv
  get <- function() x
  set <- function(m) {
    x <<- m
    inverse <<- NULL
  }
  list(get_inverse=getInverse,set_inverse=setInverse,get=get,set=set)
}


## Return inverse if cached, else calculate it and cache it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if(is.null(inv)){
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$set_inverse(inverse)
    return(inverse)
  }
  message("Cached data")
  inv
}
