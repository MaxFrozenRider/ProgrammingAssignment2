## It's my program to calculate the
## inverse of matrix

## This function creates a special "class" of marix 
## and defines some methods for this matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Creating matrix
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Getting matrix to some variable 
  getmatrix <- function() x
  ## Saving inverse matrix into cache
  setcache <- function(mean) m <<- mean
  ## Getting  inverse matrix if we have already calculate it
  getfromcache <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setcache = setcache,
       getfromcache = getfromcache)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getfromcache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if we haven't the inverse of current matrix in the cache,
  ## we must calculate it
  data <- x$getmatrix()
  m <- solve(data)
  x$setcache(m)
  m
}
