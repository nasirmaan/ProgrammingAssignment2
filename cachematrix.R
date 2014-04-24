## this program computes inverse of a square matrix. It
## consists of data structure makeCacheMatrix and
## cacheSolve function. The objective is to save computation
## by caching inverse matrix on first call to invert a matrix and use
## it in the following calls to calculate inverse of same matrix.



## makeCacheMatrix is data structure which holds
## a matrix (x) and its cache inverse (m). On matrix
## change in set() function, cache is cleared by 
## resetting m to NULL.
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setSolve <- function(inv) m <<- inv
  
  getSolve <- function() m
  
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  
}


## cacheSolve accepts makeCacheMatrix as its
## parameter and returns an inverse matrix.
## It only computes inverse matrix if it does
## not exist in cache.
cacheSolve <- function(x, ...) {

  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  if (nrow(data) == ncol(data)) {
    m <- solve(data, ...)
    x$setSolve(m)
    m
  } else {
    message("Error: provide matrix is not a square matrix!!!")
  }

}
