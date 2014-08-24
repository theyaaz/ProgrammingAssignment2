## The first function creates a cached matrix object (which is a list of functions). The second function returns by default the inverted matrix, and can access the list of functions created in the first function.

## Input a matrix, get a list of functions out.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  setinv <- function(inv) {
    m <<- inv
  }
  getinv <- function() {
    m
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) 
}

## Input the object containing your list of functions created in the first function, get an inverted matrix out.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("finding inverse and caching")
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
