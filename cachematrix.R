##The following functions are used to create a special object that stores 
##a matrix and caches its inverse. The first function, makeCacheMatrix 
##creates a special “matrix”, which is really a list containing a function to:
  
##set the value of the matrix

##get the value of the matrix

##set the value of the inverse

##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(inverse) m <<- inverse
  getMatrixInverse <- function() m
  list(set = set,
       get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getMatrixInverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrixInverse(m)
  m
}
