## cacheSolve takes in an object created by makeCacheMatrix to 
## calculate the inverse of its associated matrix. To save computation time, 
## makeCacheMatrix stores previous calculations in a cache, so that cacheSolve
## access the cache to retrieve the inverse if it has previously been computed

# makeCacheMatrix creates a list containing functions that 
# (i) set the value of a matrix
# (ii) get the value of a matrix
# (iii) set the inverse of a matrix
# (iv) get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL;
  set <- function(y) {
    x <<- y
    inv <<- NULL;
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# cacheSolve calculates inverse of matrix, after checking if the inverse is in the cache

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data...")
    return (i)
  }
  m <- x$get()
  i <- solve(m)
  x$setinv(i)
  i
}
