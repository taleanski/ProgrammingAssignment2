## The functions below are to cache the inverse of a matrix
## so that it saves time (instead of computing it repeatedly)

## The function below creates a special "matrix" object that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setCache <- function(solve) m <<- solve
  getCache <- function() m
  list(set = set, get = get, 
       setCache = setCache,
       getCache = getCache)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getCache()
  if (!is.null(m)) {
    message ("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setCache(m)
  return(m)
}


