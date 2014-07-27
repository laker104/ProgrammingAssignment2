## Put comments here that give an overall description of what your
## functions do

## The role of this function is to create a cache for this matrix

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve(x)) m <<- solve
    getinverse<- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The role of this function is to calculate the the inverse of matrix 'x' by cache or direct evaluation

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
