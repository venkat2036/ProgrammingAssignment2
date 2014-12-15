## We provide an R function that is able to cache potentially 
## time-consuming computations for finding inverse of invertible 
## square  matrices. For example, finding the inverse of a square 
## matrix is a slow operation. Also, for a very big 
## matrix, it may take very long to compute the inverse. If the 
## contents of a matrix are not changing, it may make sense to 
## cache the value of the inverse so that when we need it again, 
## it can be looked up in the cache rather than recomputed. 

## We use two functions for caching and accessing the value of
## inverse matrix: makeCacheMatrix, and cacheSolve

## makeCacheMatrix: This function creates a special “matriax, 
## which is really a list containing a function to
## 
## set the value of the vector
## get the value of the vector
## set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  // cachedInv is for holding inverse matrix
  cachedInv <- NULL
  
  # set function for storing the original data and initializing the inverse matrix 
  set <- function(y) {
    data <<- y
    cachedInv <<- NULL
  }
  
  # get function for accessing the original data
  get <- function() data
  
  # setinv for caching the inverse matrix inv
  setinv <- function(inv) cachedInv <<- inv
  
  # getinv for accessing the cached inverse matrix
  getinv <- function() cachedInv
  
  # result - special square matrix
  list(
        set = set,  
        get = get,
        setinv = setinv,
        getinv = getinv)

}


## cacheSolve: This function calculates the inverse of the special 
## “ma”trix” created with the above function. However, it first checks 
## whether the inverse has already been calculated. If so, it gets 
## the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  
  # get the inverse matrix
  cachedInv <- x$getinv()
  
  # check whether the inverse is computed
  if(!is.null(cachedInv)) {
    return(cachedInv)
  }
  
  # get the origina/input matrix
  data <- x$get()
  
  # calculte the inverse 
  cachedInv <- solve(data)
  
  # cache the inverse matrix
  x$setinv(cachedInv)
  
  # output the inverse
  cachedInv
}


