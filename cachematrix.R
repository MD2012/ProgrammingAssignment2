## Expects a solvable matrix m as input
## Encapsulates 2 private variables, 
##    m (the matrix) 
##    and i (the inverse (to be computed) of matrix m)
## Returns a list of 4 public functions: set, get, setinverse, getinverse, 
##    i.e. 2 setter/getter pairs for each of the 2 private variables m and i 
## Examples:
##    1) Invocation with a solvable matrix m
##        z<-makeCacheMatrix(m)
##        z$get() returns m
##    2) Update z with a new matrix (Setter/Getter Scoping Example)
##        using z of example 2), 
##        running z$set(m2) can be used to rebase z's private variable m, such that
##        z$get() now returns m2,
##        setinverse and getinverse work accordingly.
makeCacheMatrix <- function(m) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Requires a makeCacheMatrix() list object x
## Either 
##    a) uses the already set inverse matrix of x
##    b) or calculates the inverse matrix i of x, 
##        then stores the computed inverse matrix i back to the x list object
## Returns
##    a) the cached inverse matrix
##    b) the computed (then cached) inverse matrix
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}