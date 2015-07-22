## makeCacheMatrix NickR
## creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache

## Below was used to create an 8x8 matrix 
#mat <- matrix(rnorm(64), nrow = 8)          
#cmat <- makeCacheMatrix(mat)                  
#cmat$get()                                  
#cacheSolve(cmat)                           
#cacheSolve(cmat)   


# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  # cache will store the cached inverse matrix
  cache <- NULL
  
  # Setter for the matrix
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  # Get and set matrix
  get <- function() x
  
  # Set and get cache for the inverse of matrix
  setinv <- function(inverse) cache <<- inverse
  getinv <- function() cache
  
  # Return the matrix with our newly defined functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: attempt to get the inverse of the matrix stored in cache

cacheSolve <- function(x, ...) {
  cache <- x$getinv()
  
  # If the inverse is already calculated, return it
  if (!is.null(cache)) {
    message("cached data")
    return(cache)
  } else {
    message("data not cached")
  }
  
  # The inverse is not yet calculated, so we calculate it
  data <- x$get()
  cache <- solve(data, ...)
  
  # Cache the inverse
  x$setinv(cache)
  
  # Return the data from the matrix
  cache
}
