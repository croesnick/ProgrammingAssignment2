# makeCacheMatrix create a matrix wrapper exposing four functions:
# - getter and setter for the matrix itself
# - getter and setter for the matrix' inverse
# It is supposed to be used together witht the cacheSolve
# function which recomputes the matrix' inverse only if the
# underlying matrix itself changed.
#
# Example usage:
#   matrix <- matrix(c(1,2,3,4), nrow=2)
#   wrapper <- makeCacheMatrix(matrix)
#   cacheSolve(wrapper)

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  
  get <- function() mat
  
  set <- function(newMat) {
    mat <<- newMat
    inv <<- NULL
  }
  
  getInv <- function() inv
  
  setInv <- function(newInv) inv <<- newInv
  
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}

# cacheSolve takes a matrix wrapper (created using the above
# makeCacheMatrix function) and returns the inverse of the wrapped
# matrix. Note that for each matrix, this function only causes
# the inverse to be computed once. From the second call on, this
# function returns the cached inverse instead.

cacheSolve <- function(wrapper, ...) {
  inv <- wrapper$getInv()
  
  if (!is.null(inv)) {
    message("Using cached inverse")
    return(inv)
  }
  
  mat <- wrapper$get()
  inv <- solve(mat, ...)
  wrapper$setInv(inv)
  
  inv
}
