## The following functions would allow retrieval of cached
## calculations of matrix's inverse, if any. If there is no
## cached calculation, the calculated inverse will be stored.

## makeCacheMatrix function - create a list of functions to set matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function - solves for matrix's inverse if not cached previously.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
