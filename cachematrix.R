## The two functions below are used together to compute and cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #Set value of x
    x <<- y
    m <<- NULL #reset m
  }
  get <- function() x #Get value of x
  setinv <- function(inv) m <<- inv #Set value of m
  getinv <- function() m #Get value of m
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}


## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix. If the inverse has already been calculated, it is retrieved from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) { #Get inverse from cache
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) #Compute inverse
  x$setinv(m)
  m
}
