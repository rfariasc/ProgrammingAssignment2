## Memoization of matrix inversion

## Create a modified matrix object that stores the value of the inverse once calculated

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function tries to get the inverse stored in the cheched matrix if it doesn't exists, 
## calculate and store it


cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


# Example usage

# Define a matrix
A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)

# Conventional usage
solve(A)

# Using cached version
newA = makeCacheMatrix(A)

# First time is calculated
cacheSolve(newA)

# Second time uses the cache
cacheSolve(newA)

