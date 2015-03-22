# Calculating the inverse of a matrix can be a costly operation that there is value in
# storing the results in a cache for quicker lookup during subsequent calls.

# makeCacheMatrix accepts a matrix as an argument and returns a list of functions 
# that allow access to the function and it's inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  
  #Return a list that exposes the functions above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve retrieves the inverse of a matrix if it is cache, 
# otherwise it calculates the inverse, updates the cache, and then returns the inverse.
# the function takes in a cacheMatrix list that is returned from the makeCacheMatrix function.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}

#Example code execution:

#> source("cachematrix.R")
#> initial <- matrix(c(1, 1, 1, 3, 4, 3, 3, 3, 4), nrow=3, ncol=3)
#> cacheMatrix <- makeCacheMatrix(initial)
#> initial
#[,1] [,2] [,3]
#[1,]    1    3    3
#[2,]    1    4    3
#[3,]    1    3    4
#> cacheSolve(cacheMatrix)
#[,1] [,2] [,3]
#[1,]    7   -3   -3
#[2,]   -1    1    0
#[3,]   -1    0    1
#> cacheSolve(cacheMatrix)
#getting cached data
#[,1] [,2] [,3]
#[1,]    7   -3   -3
#[2,]   -1    1    0
#[3,]   -1    0    1