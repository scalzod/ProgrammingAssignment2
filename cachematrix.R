## Put comments here that give an overall description of what your
## functions do:

#### Learning to cache a potentially time consuming function and then perform a function using the cache.
#### e.g. cache the inverse of a matrix inversion for future operations.

## Write a short comment describing this function:
##makeCacheMatrix <- function(x = matrix()) {
##}

####This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
##cacheSolve <- function(x, ...) {
##        ## Return a matrix that is the inverse of 'x'
##}

####This function calculates the inverse of the special "matrix" created with the above function. 
####However, it first checks to see if the inverse has already been calculated. 
####If so, it gets the inverse from the cache and skips the computation.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- inverse(data, ...)
  x$setinverse(i)
  i
}

#### To paraphrase D. Knuth:
#### "Beware of bugs in the above code; I have only 'typed it', not tried it."
