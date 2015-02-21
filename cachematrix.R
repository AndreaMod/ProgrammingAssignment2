## The following two functions are used to create a special object 
## that stores a matrix and cache's its inverse

## This first function creates a list containing functions to set/get the value of the matrix and to set/get the value of the inverted matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The second function calculate the inverse of the List created with the above function. It first check if the inverted Matrix has already been calculated, in that case gets the matrix from the cache, otherwise it calculates the inverse of the matrix and store the value in the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
