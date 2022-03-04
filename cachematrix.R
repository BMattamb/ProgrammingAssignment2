## this function caches a matrix (a) 

makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  data <- function(y){
    a <<- y
    inverse <<- NULL
  }
  get <- function()a
  datareverse <-function(inverse) inv <-inverse
  getinverse <- function() inv
  list(data = data, get = get, datareverse = datainverse, getinverse = getinverse)
}

## this function returns the inverse of the matrix cached with makeCacheMatrix function. If unavailable, cacheSolve computes and caches it before returning it.

cacheSolve <- function(a, ...) {
  inv <= a$getinverse()
  if(!is.null(inv)){
    message("computing and caching data")
    return(inv)
  }else{
    info <- a$get()
    inv <- solve(info)
    a$setreverse(inv)
    inv
  }
}
