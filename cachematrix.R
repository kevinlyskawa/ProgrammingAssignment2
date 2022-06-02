## To 'Cache' is to get the inverse of a matrix. It is often beneficial as 
## taking the inverse of matrices is cosstly

## Making a matric called, "makeCacheMatrix"
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function below, "cacheSolve" takes the inverse of the matrix from 
## the above function, "makeCacheMatrix"

cacheSolve <- function(x, ...) {
  ## Output is inverse of x
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
