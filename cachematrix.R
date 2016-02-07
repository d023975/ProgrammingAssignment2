## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## it is just the same as the vector example but for a matrix -> see vector explanation
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse (solve)
##get the value of the inverse (solve)
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}

## Write a short comment describing this function
##The following function calculates the inverse of the special "matrix" created with the above function. 
##However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setsolve function.

##How to test:
##  
##> source("cachematrix.R")
##> a <- makeCacheMatrix()
##> c=rbind(c(1, -1/4), c(-1/4, 1))  
##> a$set(c)
##> a$get()
##[,1]  [,2]
##[1,]  1.00 -0.25
##[2,] -0.25  1.00
##> cacheSolve(a)
##[,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667
##> cacheSolve(a)
##getting cached data
##[,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the solve of 'x'
  
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i  
  
  
}