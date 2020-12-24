
## Inverse matrix computation cache mechanism 

## returns an interface to interact with a matrix, 
## caches the inverse and invalid the cache when the 
## matrix changes.

makeCacheMatrix <- function(x = matrix()) {
  inner.matrix <- x 
  cached.matrix.inverse <- NULL
  getMatrix <- function () inner.matrix 
  setMatrix <- function (new.matrix) {
    cached.matrix.inverse <<- NULL
    inner.matrix <- new.matrix 
  }
  getCachedInverse <- function () cached.matrix.inverse
  setCachedInverse <- function (new.inverse) 
    cached.matrix.inverse <<- new.inverse 
  invisible(list(
    getMatrix = getMatrix, 
    setMatrix = setMatrix, 
    getCachedInverse = getCachedInverse, 
    setCachedInverse = setCachedInverse
  ))
}

## solve the inverse of a matrix. If the object has a valid inverse 
## stored in the cache, skips the computation and returns the cached
## inverse 

cacheSolve <- function(x, ...) {
  cached.inverse <- x$getCachedInverse()
  if (!is.null(cached.inverse)) {
    message("return cached data")
    return (cached.inverse)
  }
  inner.matrix <- x$getMatrix()
  cached.inverse <- solve(inner.matrix, ...)
  x$setCachedInverse(cached.inverse)
  cached.inverse
}
