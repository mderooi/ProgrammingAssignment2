## Put comments here that give an overall description of what your
## functions do

# This script contains two functions being makeCacheMatrix and cacheSolve. Function makeCacheMatrix creates an R boject that stores a matrix and its inverse.  
# Function cacheSolve requires an argument that is returned by makeCacheMatrix to retrieve the inverse from the cached value that is stored in the makeCacheMatrix objects environment

## Write a short comment describing this function
# makeCacheMatrix is a function that builds a set of functions related to data matrix inversion and returns the functions within a list to the parent environment
# so it is the preparation for cacheSolve as it stores the objects in the makeCacheMatrix environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve is a function that retrieves the inverse from an object from makeCacheMatrix
# so here the inverse function is actually executed and the outcome printed

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
}



## testing
df2 <- matrix(c(5,6,7,8),2,2)
solve(df2)

df2.test <- makeCacheMatrix(df2)
cacheSolve(df2.test)


