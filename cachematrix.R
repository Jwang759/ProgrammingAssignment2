## This function, makeCacheMatrix creates a special "matrix", which contains following functions:
## 1. set the original matrix
## 2. get the original matrix
## 3. set the inverse matrix 
## 4. get the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function, cacheSolve, firstly checks to see if the inverse matrix has been calculated. If so, it gets the inverse matrix from the cache and skip the computation.
## If not, it calculates the inverse matrix and sets the inverse matrix in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
