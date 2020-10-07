## Calculate the inverse of a matrix
## Caches the result obtained the first time

## Return a list with utility functions for a matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(mtx) {
    x <<- mtx
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getmean()
  
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setmean(m)
  m
}
