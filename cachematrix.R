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

  # Define the matrix proper. Everytime we (re-)define the matrix, the cache is
  # cleared to force recalculation of the inverse.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  # Simply return the matrix
  get <- function() x

  # Cache the inverse of the matrx in variable `m`.
  setinv <- function(inv) m <<- inv

  # Return the contents of the cache (can be an inverted matrix or NULL)
  getinv <- function() m

  # Now we return a list with 4 elements ('set', 'get', 'setinv' and 'getinv') and
  # link them to the appropriate function defined above.
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


# `cacheSolve` returns the inverse of a matrix created via `makeCacheMatrix`.
# If the inverse is already cached (was previously computed), it just returns it.
# Otherwise, the inverse matrix is computed, saved to the cache and returned.
#
# Note: we assume matrices passed as argument are invertible.

cacheSolve <- function(x, ...) {
  # Get the contents of the cache
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
