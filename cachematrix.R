## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# this function works like a class, it creates a list
# that contains 4 member functions: set, get, setsolve, getsolve
makeCacheMatrix <- function(x = matrix()) {
  inverso <- NULL # SEtting to null for a future value
  set <- function(y) { #define vector
    x <<- y
    inverso <<- NULL
  }
  get <- function() x #returns vector
  setsolve <- function(solve) inverso <<- solve
  getsolve <- function() inverso
      # return a list that contains these functions, so that we can use
      # makeCacheMatrix object like these
      # x <- makeCacheMatrix(testmatrix)
      # setsolve(newmatrix) # to change matrix
      # getsolve # to get the setted matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
 # null if uncalculed
  inverso <- x$getsolve()
  if(!is.null(inverso)) {
    message("getting cached data")
    return(inverso)
  }
  data <- x$get()
  inverso <- solve(data, ...)
  x$setsolve(inverso)
  inverso
}

##Prove
# probe
# to generate a matrix

testing <- matrix(rnorm(16,100,10),4,4)
Cache <- makeCacheMatrix(testing)
toprove <- cacheSolve(Cache)
toprove
