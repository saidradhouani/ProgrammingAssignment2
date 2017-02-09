# The goal is to calculate the inverse matrix and retrieve it from from the cache.
# These are two functions, the first inverses the matrix, and the second retrieves it from the cache

#The first function, makeCacheMatrix takes a matrix "x" as input and creates a new matrix "m"
#that can cache the inverse of "x". It has four functions
# 1. set: it sets the value of the matrix "x", which is stored in the main function 
# 2. get: returns the value of the orignal matrix "x"
# 3. setinverse: it uses the R function "solve" to set the inverse of the matrix "x" and store it in "m"
# 4. getinverse: it returns the value of matrix "m", which is the inverse of the orginial matrix "x"



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#"cacheSolve" is a function that create the inverse of a given matrix "x"
#First, "cacheSolve" verifies whther "m2 is not null, which means that it already has the 
#the inverse matrix (calculated within "makeCacheMatrix"). If that's the case, "cachesolve" will
#return the value of "m" from the cache. 
#If "m" is empty (i.e, the inverse has not been created), the function "get" from "makeCacheMatrix" is used
#to get the value of "x". Then the fucntion "solve" is called to claculate the inverse, finally "x$setmean(m)"
# is used to store the inverse in "m" and return it



cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}