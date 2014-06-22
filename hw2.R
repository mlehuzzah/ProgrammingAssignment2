## to use these functions:
## a a matrix
## b<-makeCacheMatrix(a)
## cacheSolve(b)



## input a (nonsingular, square) matrix
## output a list of four functions
makeCacheMatrix<- function(x = matrix()) {
  m <- NULL
  ## function set stores the input matrix as x
  ## if an input matrix being stored, then matrix inverse "m" is cleared
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  ## function get returns the stored matrix x
  get <- function() x
  
  ## function setInverse stores input to m
  setInverse <- function(z) m <<- z
  
  ## function getInverse retrieves m
  getInverse <- function() m

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getInverse()

  ## inverse stored in m; if m is not null, will return value of m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if no cached inverse, retrieves the original matrix
  data <- x$get()
  
  ## calculates the inverse of the matrix
  m <- solve(data, ...)

  ## stores (caches) the inverse of the matrix
  x$setInverse(m)
  m
}