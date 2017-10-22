## Put comments here that give an overall description of what your
## functions do

# clean up workspace, remove all possibly existing elements to create clean environment
rm(list=ls())


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # create object m with value NULL
  m <- NULL
  # set the matrix: assign y to x and NULL to m
  set <- function(y) {
        x <<- y
        m <<- NULL
  }
  # get the matrix: return x
  get <- function() x
  # set the inverse: assign argument inverse to m
  setinverse <- function(inverse) m <<- inverse
  # get the inverse: return m 
  getinverse <- function() m
  # returns a list with set, get, set inverse and get inverse 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
# END of makeCacheMatrix #################

## This function gets the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  # create object m get the inverse of the matrix x
  m <- x$getinverse()
  # if the value of m is not null: print message and return m 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # create object data get the matrix x
  data <- x$get()
  # Compute the inverse of a square matrix data and save results to m
  m <- solve(data, ...)
  # set inverse of matrix x to m
  x$setinverse(m)
  # return m
  m
}
# END of cacheSolve #################

# create new object of type makeCacheMatrix called myX
myX <- makeCacheMatrix()

# create test matrix of size 3x3 called myMatrix
myMatrix <- matrix (c(2,3,4,1,2,5,6,2,3), nrow =3, ncol=3)

# set the matrix myMatrix to the object myX
myX$set(myMatrix)

# perform matrix inversion of the matrix stored in the object myX
cacheSolve(myX)

# do the same thing again: this time, the matrix should be taken from storage and not inverted again
cacheSolve(myX)

# check if inversion was successful: multiply inverse with matrix and check if result is unity matrix
cacheSolve(myX) %*% myMatrix 
