# Programming Assignment 2
# R Programming course (rprog-012)
# António Pedro Cunha (pwseo)


# Function `makeCacheMatrix` creates matrices that can cache their own inverse.
# Since it can be computationaly intensive to calculate the inverse of a matrix,
# these 'special' matrices can be useful in situations where there's the need to
# repeatedly calculate their inverse.

makeCacheMatrix <- function(x = matrix()) {
  # The cache is always empty in the beginning
  m <- NULL

  # This function allows the programmer to set the content of the matrix (ie.
  # replace the current matrix with a new one).
  # Every time the matrix is replaced, the inverse matrix must be recalculated,
  # and so we empty the cache (set it to NULL)
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  # Simply return the matrix
  get <- function() x

  # Cache the `inv` matrix in `m`
  # `inv` is the result of running `solve(x)` (handled by the `cacheSolve`
  # function below).
  setinv <- function(inv) m <<- inv

  # Simply return the inverted matrix, if it's cached.
  # If the cache is empty, return NULL
  getinv <- function() m

  # Now we return a list with 4 elements ('set', 'get', 'setinv' and 'getinv') and
  # link them to the appropriate function defined above.
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


# Function `cacheSolve`
# This function takes a special matrix (as created by the `makeCacheMatrix` function
# defined above) and returns its inverse.
# Since these special matrices can cache their inverse, this function first tries to
# return the cached result (thus eliminating the need to compute the result).
# If, however, the cache is empty, `cacheSolve` will compute the inverse (via the
# `solve` function, store the result in the cache and return it.
#
# Note: we assume matrices passed as argument are invertible.

cacheSolve <- function(x, ...) {
  # Get the cache content
  m <- x$getinv()
  if (!is.null(m)) {
    # If the cache is not empty (ie. is not NULL)), print a message signaling we're
    # returning the cached matrix, return it and exit the function at this point.
    message("getting cached data")
    return(m)
  }

  # If, on the other hand, the cache is empty (m == NULL), run `solve` on the original
  # matrix data (obtained via `x$get()`) and store it in the cache (via `x$setinv(n))`).
  m <- solve(x$get(), ...)
  x$setinv(m)

  # Return the computed inverse.
  m
}
