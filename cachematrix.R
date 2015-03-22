# Programming Assignment 2
# R Programming course (rprog-012)
# António Pedro Cunha (pwseo)


# Function `makeCacheMatrix` creates matrices that can cache their own inverse.
# Since it can be computationaly intensive to calculate the inverse of a matrix,
# these 'special' matrices can be useful in situations where there's the need to
# repeatedly calculate their inverse.

makeCacheMatrix <- function(mat = matrix()) {
  # The cache is always empty in the beginning
  cache <- NULL

  # Create the matrix
  set <- function(x) {
    mat <<- x
    cache <<- NULL    # (re-)defining the matrix (`mat`) forces computation of inverse
  }

  # Get the matrix
  get <- function() mat

  # Store the inverted matrix in cache
  setinv <- function(inv) cache <<- inv

  # Get the cache contents (can be an inverted matrix or NULL)
  getinv <- function() cache

  # Now we return a list with 4 elements (`set`, `get`, `setinv` and `getinv`) and
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
# Assumption: every matrix passed as argument is square and invertible

cacheSolve <- function(mat, ...) {
  # Get the contents of the cache
  cache <- mat$getinv()

  # If the cache isn't empty, return the cached inverted matrix and stop
  if (!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }

  # If the cache is empty, compute the inverse of the matrix and cache it.
  cache <- solve(mat$get(), ...)
  mat$setinv(cache)

  # Return the computed inverse.
  cache
}
