## David Engel
## 6/20/2014
## This file creates two main functions which essentially build a "CacheMatrix"  
## object from a regular numerical matrix. A cacheMatrix object can return an  
## inverse matrix of the original matrix, while at the same time caching the  
## inverse (which can be computationally intense to calculate) to improve  
## performance in the case the inverse is called for repeatly during run-time

## Function: makeCacheMatrix
## -------------------------
## Takes as a parameter a numerical matrix that is capable of being inverted 
## (as the assigment dictates, we do no error-checking here) and returns a 
## "CacheMatrix" object, a list containing the essential get/set functions 
## (either get or set the original matrix x) and get/setinverse functions 
## (either get or set the inverse matrix of the original matrix)
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  ### sets the matrix, nulls out the matrix inverse 
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  ### returns the matrix
  get <- function() x
  ### sets the inverse of the matrix. Performs no calculations
  setinverse <- function(inverse) inverseMatrix  <<- inverse
  ### returns the inverse of the matrix
  getinverse <- function() inverseMatrix 
  
  ## returns the list object containing the above functions
  list( 
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Function: cacheSolve
## --------------------
## Takes a "CacheMatrix" object x, and returns a matrix inverse of the 
## matrix in x. The first time this function is called the matrix 
## inverse will be calculated as well as cached. All subsequent calls 
## to the function will simply return the cached inverse matrix (a brief
## message is also printed to the console letting the user know that the
## cached matrix is being used).
cacheSolve <- function(x, ...) {
  ### we get x's inverse, if it isn't null we print a message and return the inverse
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ### we use the solve function to calculate the inverse of x's matrix, set it 
  ### to cache it and then return it
  inv <- solve(x$get(), ...)
  x$setinverse(inv)
  inv
}
