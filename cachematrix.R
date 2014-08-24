# Code below implements calculation of inverse matrixes. Every result
# is stored in cache in order to avoid duplication of work.


# makeCacheMatrix returns list of 4 sub-functions which are used
# to implement caching.
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setmatrix <- function(matr) cache <<- matr
  getmatrix <- function() cache
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


# cacheSolve returns inverse matrix. Results are cached in order to avoid calculating same
# result again.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cache <- x$getmatrix()
  if(!is.null(cache)) {
    message("found some data in cache!")
    return(cache)
  }
  data <- x$get()
  cache <- solve(data, ...)
  x$setmatrix(cache)
  cache
}
