## Function to cache matrix inversion

## TESTING
##  Create an object to store a matrix
##    cacheableMatrix <- makeCacheMatrix(rbind(c(10,20), c(30,40)))

## Make a call to cacheSolve to find the inverse of the matrix
##    inverse1 <- cacheSolve(cacheableMatrix)

## Make the cacheSolve call again to to calculate the inverse of the matrix
##    inverse2 <- cacheSolve(cacheableMatrix)
##  this time the cached value from the previous call will be returned


## The following function makeCacheMatrix provide an interface to cache a matrix
## and access the cached inverse value 

makeCacheMatrix <- function(x = matrix()) {
  cachedinverse <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    cachedinverse <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse of the matrix
  setCacheInverse <- function(inverse) cachedinverse <<- inverse
  
  # get the value of the inverse of the matrix
  getCacheInverse <- function() cachedinverse
  
  list(set = set, get = get,
       setCacheInverse = setCacheInverse,
       getCacheInverse = getCacheInverse)
}


## This function cacheSolve returns the inverse of the matrix after 
## checking the cache to see if it has already been calculated and cached.
## If inverse is already been calculated it returns the cached value
## otherwise it will calculate the inverse, cache it and returns the value

cacheSolve <- function(x, ...) {
  ## get the value of the inverse that is already been stored in the cache
  invValue <- x$getCacheInverse()
  
  ## check to see whether the inverse is already been calculated
  ## if it is not present in cache invValue will be NULL
  if(!is.null(invValue)) {
    message("getting cached data")
    # return cached inverse value of the matrix x
    return(invValue)
  }
  
  ## get the value of the matrix
  data <- x$get()
  
  ## calculate the inverse
  invValue <- solve(data, ...)
  
  ## store inverse in the cache
  x$setCacheInverse(invValue)
  
  ## return the matrix that is the inverse of the matrix x
  invValue
}