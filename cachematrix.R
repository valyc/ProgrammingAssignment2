# ## # ## #
# These two functions calculate and cache the inverse of a matrix.

# This function creates an interface to access(get) and cache(set)
# the contents of a matrix and the inverse of original matrix.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(inv) inverse <<- inv
  get_inverse <- function() inverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}
## This function computes the inverse of the special "matrix" 
#  returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inv_matrix of 'x'
  inverse <- x$get_inverse()
  # check if inverse matrix was already calculated and 
  # retrieve the content from the cache.
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # else calculate the inverse matrix
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse
}