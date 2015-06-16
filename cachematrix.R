## These functions are used to create a special matric and cache the inverse

## makeCacheMatrix function create a square matric for caching the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function compute the inverse of the matric return by akeCacheMatrix function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting inverse matric")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
