## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  set.inv.mat <- function(solve.mat) matinv <<- solve.mat
  get.inv.mat <- function() matinv
  list(set = set, get = get,
       set.inv.mat = set.inv.mat,
       get.inv.mat = get.inv.mat)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matinv <- x$get.inv.mat()
  if(!is.null(matinv)) {
    message("getting cached data")
    return(matinv)
  }
  data <- x$get()
  matinv <- solve(data, ...)
  x$set.inv.mat(matinv)
  matinv
}
# 