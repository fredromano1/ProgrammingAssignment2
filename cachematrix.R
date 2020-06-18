## The aim is to avoid having to repeatedly compute a matrix inverse.
## makeCacheMatrix produces it automatically.
##cacheSolve either gets the solution if it was previously computed or launches makeCacheMatrix.

## This first function creates a cache inverse of a matrix in a special object.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inver <<- solve(x)
  getInverse <- function() inver
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This second function works either gets the cached inverse if it already exists, or computes it.

cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
  if (!is.null(inver)){
    message("Getting cached data")
    return(inv)
  }
    
  data <- x$get()
  inver <- solve(x)
  x$setInverse(inver)
  return(inver)
  
}
