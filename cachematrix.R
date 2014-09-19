## Given an invertible matrix computes the inverse and stores both in cache so that computations need only occur once.


## Given a matrix x, returns a "matrix object" and seeds it with the matrix x.  This "object" is a list of 4 functions to change / retrieve both the value of matrix x and its inverse, inv within the cache
makeCacheMatrix <- function(x = matrix()) {  # x is an invertible matrix
  inv <- NULL  #inv is the inverse matrix of x
  set <- function(y) {   #set the value for x
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  fncts <- list(set = set, get = get,   #list of functions to pass to cacheSolve
       setinverse = setinverse,
       getinverse = getinverse)
}

## Given a matrix "object" x either returns inverse if it is stored in cache or computes the inverse and then updates the value in cache
cacheSolve <- function(x, ...) { #x is a special "vector" of type created by makeCacheMatrix
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)}
  mat <- x$get()
  inv <- solve(mat)
  x$setinverse(inv)
  inv
}


