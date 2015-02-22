## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  inverso <- NULL # SEtting to null for a future value
  set <- function(y) { #define vector
    x <<- y
    inverso <<- NULL
  }
  get <- function() x #returns vector
  setsolve <- function(solve) inverso <<- solve
  getsolve <- function() inverso
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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

testing <- matrix(rnorm(16,100,10),4,4)
Cache <- makeCacheMatrix(testing)
toprove <- cacheSolve(Cache)
toprove
