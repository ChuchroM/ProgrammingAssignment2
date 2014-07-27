## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## MakeCacheMatrix creates a matrix object, and inverse, put into cache. 
makeCacheMatrix <- function(x = matrix()) {
inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## CacheSolve calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead  find it in the cache and return it.
## The inverse matrix will be not calculate again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("get cached matrix inverse")
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}
