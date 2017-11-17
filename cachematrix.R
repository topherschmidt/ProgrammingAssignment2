## makeCacheMatrix creates a function that will create an object
## with of type makeCacheMatrix() and local variable m1 & x which 
## stores a matrix.  
## There are 4 methods created to interact with the matrix,
## 1.) set - allows the user to set the values of matrix x
## 2.) get - retuns the values contined within matrix x
## 3.) setinverse - allows the user to set the values of 
##     inverse matrix m1
## 4.) getinverse - returns the values of inverse matrix m1


makeCacheMatrix <- function(x = matrix()) {
  m1 <- NULL
  set <- function(y) {
    x <<- y
    m1 <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m1 <<- inverse
  getinverse <- function() m1
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function that accepts an object of type
## makeCacheMatrix() and calculates the inverse of the matrix
## stored in variable x using the solve function.  If the 
## cacheSolve function has already been called and the inverse 
## has already been calculated and stored in the object, the cached
## result is returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m1 <- x$getinverse()
  if(!is.null(m1)) {
    message("getting cached data")
    return(m1)
  }
  data <- x$get()
  m1 <- solve(data, ...)
  x$setinverse(m1)
  m1
}
