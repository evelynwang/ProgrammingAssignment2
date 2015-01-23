## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y){
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, A) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  if(!is.null(inv) && x$get() == A){
    message("getting cached data")
    return(inv)
  }
  else if(!is.null(inv) && x$get() != A){
    message("the matrix has been changed")
    message("inverse of the matrix needs to be recalculated")
    inv = solve(A)
    x$set(A)
    x$setinv(inv)
    inv
  }
  data = x$get()
  inv = solve(data)
  x$setinv(inv)
  inv
}
