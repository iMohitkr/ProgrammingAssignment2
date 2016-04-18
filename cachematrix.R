## makeCacheMatrix function can be used to make a matrix object that
## has its inverse cached. cacheSolve function can be used to solve
## for the makeCacheMatrix object's inverse and cache it for future
##use

## makeCacheMatrix function is taking a matrix as its input and helps
## to create an object whose inverse can be cached

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


## cacheSolve is taking input the makeCacheMatrix objects and it 
## returns the inverse of the same while also caching it for future
## use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
