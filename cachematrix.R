## Set of functions to solve the inverse of a matrix. 
## They also save the result (cache) to make it more efficient 
## in case that we need to repeat this operation several times

################################################################
## makeCacheMatrix: Builds a cacheMatrix from an existing matrix
## INPUT:
##    x - An invertable matrix
## OUTPUT: 
################################################################
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

################################################################
## cacheSolve: Computes the inverse of a cacheMatrix.
##  If the inverse had already been calculated it returns the
##  cached inverse
## INPUT:
##    x - A cacheMatrix
## OUTPUT:
##    The inverse of the cacheMatrix 'x'
################################################################
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinv(inv)
  inv
}