## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it repeatedly.
## The following pair of functions cache the inverse of a matrix and then retreive it .
## The inverse if the matrix has not changed.


## makeCacheMatrix: This list of functions creates a special "matrix" object that can cache its inverse.
## The functions are intended to:

  ## 1.  set the value of the matrix
  ## 2.  get the value of the matrix
  ## 3.  set the value of the inverse matrix
  ## 4.  get the value of the inverse matrix

## The value of the inverse matrix is computed in the second function (cacheSolve)

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## cacheSolve: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

## Examples:
  ## 1. Simple example with expected results
amatrix <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
amatrix$get() # Returns the original matrix
  #     [,1] [,2]
  #[1,]    1    3
  #[2,]    2    4
cacheSolve(amatrix) # Computes, caches and returns the matrix inverse
  #     [,1] [,2]
  #[1,]   -2  1.5
  #[2,]    1 -0.5
cacheSolve(amatrix) # Returns chached matrix with the message 'getting cached data'
  #getting cached data
  #     [,1] [,2]
  #[1,]   -2  1.5
  #[2,]    1 -0.5

  ## 2. Performance test(to demonstrate that inverse is comming from the chache)
matrixData <- matrix(stats::rnorm(90000), nrow=300, ncol=300)
ptest <- makeCacheMatrix(matrixData)
system.time(t1 <- cacheSolve(ptest)) # first time no cache
  #user  system elapsed 
  #0.031   0.001   0.033 
system.time(t2 <- cacheSolve(ptest)) # second time using cached data
  #user  system elapsed 
  #0.000   0.000   0.001 
