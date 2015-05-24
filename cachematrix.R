## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inv_glob <- NULL
  set <- function(y) {
    x <<- y
    inv_glob <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_glob <<- inverse
  getinverse <- function() inv_glob
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# Next function computes the inverse, it returns already computed one, or if there is no 
# such one, it computes one and stores it for future use

cacheSolve <- function(x, ...) {
  
    cacheSolve <- function(x, ...) {
      inv_glob <- x$getinverse()
      if(!is.null(inv_glob)) {
        message("getting cached data")
      return(inv_glob)
    }
    data <- x$get()
    inv_glob <- solve(data)
    x$setinverse(inv_glob)
    inv_glob
  }
  
}
