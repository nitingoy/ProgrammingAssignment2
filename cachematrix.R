## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#We have 4 operations here. Global variable X is origional matrix and matx is invereted matrix.
# set and get is to set origional matrix.
# setinverse and getinvesre is for inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  mtrx <- NULL 
  set <- function(arg) { 
    x <<- arg 
    mtrx <<- NULL 
  } 
  get <- function() x 
  setinverse <- function(inverse) mtrx <<- inverse 
  getinverse <- function() mtrx 
  list(setinverse = setinverse,  getinverse = getinverse,set = set, get = get) 
}


## Write a short comment describing this function
# here we are checking if invesre is available. 
# if invrse is available we return it. otherwise we will get inverse and set inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mtrx <- x$getinverse() 
  if(!is.null(mtrx)) { 
    return(mtrx) 
  } 
  data <- x$get() 
  m <- solve(data, ...) 
  x$setinverse(mtrx) 
  mtrx 
}
