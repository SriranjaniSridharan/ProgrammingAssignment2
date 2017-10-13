## Finds inverse of matrix and cache the same. 

## Function makeCacheMatrix creates an environment with 4 functions and holds the cached inverse matrix in variable i and the actual matrix in variable x

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinvmat <- function(invmat) i <<- invmat
  getinvmat <- function() i
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}


## if inverse matrix is already calculated and cached it returns the same otherwise it finds the inversematrix and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinvmat()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinvmat(i)
  i
}
