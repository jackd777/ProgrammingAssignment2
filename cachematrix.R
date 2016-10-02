
## The makeCacheMatrix function creates a special matrix object that can cache its 
## inverse square.

makeCacheMatrix <- function(x = matrix()) {
  InvSqr <- NULL
  set <- function(y) {
    x <<- y
    InvSqr <<- NULL
  }
  get <- function() x
  setInvSqr <- function(InverseSquare) InvSqr <<- InverseSquare
  getInvSqr <- function() InvSqr
  list(set = set, get = get,
       setInvSqr = setInvSqr,
       getInvSqr = getInvSqr)
  
}


## The cacheSolve function checks if the inverse of a special matrix object is in cache,
## and if it is in cache then it recalls from cache, if cache is empty then
## the function calculates the inverse of the special matrix object and caches it.

cacheSolve <- function(x, ...) {
  InvSqr <- x$getInvSqr()
  if(!is.null(InvSqr)) {
    message("getting cached data")
    return(InvSqr)
  }
  data <- x$get()
  InvSqr <- solve(data)
  x$setInvSqr(InvSqr)
          
  InvSqr   ## Return a matrix that is the inverse of 'x'
}
