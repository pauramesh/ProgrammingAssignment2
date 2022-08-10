## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function(y) {
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



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

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
  
  ## Return a matrix that is the inverse of 'x'
}



# ###............................................................................
# Testing the function 
a<-- matrix(1:4, nrow=2, byrow=TRUE)
# Return innverse of the matrix without using the function
solve(a)

a1<- makeCacheMatrix(a)
# Return inverse from computation (using the function)
cacheSolve(a1)

# Return inverse from cache
cacheSolve(a1)