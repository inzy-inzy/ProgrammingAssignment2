##Enter matrix in function 'makeCacheMatrix'

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## function cachesolve will check cached data for inverser of matrix, else inverse the matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
