## We write 2 functions: 
## makeCacheMatrix creates a structure (a list) where a matrix inverse is eventuall
## stored, as well as basic to to read the initial matrix and its inverse or NUL if it is not yet computed
##
##cacheSolve returns the inverse of the matrix if it is already computed and stored, 
## otherwise  it computes it using the function solve, and stores it in the cache matrix structure

#CacheMatrix creation, it returns 4 functions to set and get the initial matrix and its inverse, or NULL 
#if not yet comuted with the other function cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  # inv is set to NULL at the creation of the cacheMatrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv<<- NULL
  }
  #a function to return the initial matrix
  get <- function() x
  #a function to set the inverse matrix
  setINV <- function(IM) inv <<- IM
  # a function to return the inverse matrix
  getINV <- function() inv
  #the set of functions that give access to the matrix and its inverse
  list(set = set, get = get,
       setINV = setINV,
       getINV = getINV)  
}



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  IM <- x$getINV()
  #if the inverse is already computed, we simply return it
  if(!is.null(IM)) {
    message("getting cached data")
    return(IM)
  }
  #otherwise, we compute the inverse and store it in the cache matrix structure
  mat <- x$get()
  IM <- solve(mat)
  x$setINV(IM)
  IM
}
