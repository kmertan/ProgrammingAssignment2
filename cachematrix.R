## Put comments here that give an overall description of what your
## functions do

## We make a special "matrix" using makeCacheMatrix that has the four properties in list. This allows us to cache the inverse for future use in cacheSolve

## Write a short comment describing this function
## The first function takes a normal matrix and makes getters and setters for the value and inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
## cacheSolve either returns the cached inverse or computes it and stores it in the special "matrix" for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)){
      message("getting cached data")
      return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}

#testMatrix <- matrix(c(1, 2, 3, 0, 1, 4, 5, 6, 0), 3, 3, byrow = T)
#newX <- makeCacheMatrix(testMatrix)
#cacheSolve(newX)
#cacheSolve(newX) ## This time we see "getting cached data"