##Enter matrix in function 'makeCacheMatrix'.

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

## The function 'cachesolve' will check cached data for inverse of matrix, else inverse the matrix.

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


-------------------------------------------------------------------------------------------------------------------
##Example

> n <- matrix(rnorm(16),4,4)
> n
           [,1]        [,2]       [,3]       [,4]
[1,]  0.6584612  0.19242736  1.0695441  0.2611085
[2,] -0.6206371 -0.92256603 -0.2483907 -3.1200360
[3,]  1.5254982 -0.64952841 -0.8170107  0.5071488
[4,]  0.9193741  0.01190595 -0.8926503 -0.4170318

> x<-makeCacheMatrix(n)
> cacheSolve(x)
           [,1]         [,2]        [,3]       [,4]
[1,]  0.5109871  0.004762889  0.15324581  0.4706622
[2,]  0.2322531 -0.314501717 -1.00057382  1.2815811
[3,]  0.6324775  0.111594733  0.02130065 -0.4129942
[4,] -0.2206731 -0.237345508  0.26368110 -0.4396968


--------------------------------------------------------------------------------------------------------------------
