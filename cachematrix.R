## makeCacheMatric creates a list that contains four items:

## A set function, that sets the value of a matrix in the parent
## environment, and sets the value of the inverse of that matrix to
## NULL in the parent environment.

## A get function, that gets the value of the matrix.

## A setinv functions, that sets the value of the inverse matrix
## in the parent environment.

## A getinv function that returns the value of the inverse matrix.

makeCacheMatrix <- function(m = matrix()) {
  minv <- NULL
  set <- function(y) {
    m <<- y
    minv <<- NULL
  }
  get <- function() m
  setinv <- function(n) minv <<- n
  getinv <- function() minv
  list(set= set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns the cached value of the inverse if one exists
## and otherwise calculates the inverse, sets the value in the 
## parent environment, and then returns the inverse.

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  if(!is.null(minv)) {
    return(minv)
  }
  data <- x$get()
  minv <- solve(data)
  x$setinv(minv)
  minv
}