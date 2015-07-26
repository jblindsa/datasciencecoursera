## makeCacheMatrix creates a list of functions that perform the tasks of setting and retrieving the values 
#  of the matrix and its inverse. cacheSolve solves for the inverse of the matrix. If it has already been solved 
#  cacheSolve retrieves the value from the cache (it is stored in the parent environment).

## makeCacheMatrix creates a list of functions that:
#  1) sets the value of the matrix passed as an argument
#  2) gets the value of the matrix passed as an argument
#  3) sets the value of the inverse of the matrix (in the parent environment so it is cached)
#  4) gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 set <- function(y){
   x <<- y
   inv <<- NULL
 }
 get <- function() x
 setinverse <- function(inverse) inv <<- inverse
 getinverse <- function() inv
 list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}

######## this is one of the existing functions in the stub file
makeVector <- function(x = numeric()) {
 m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## cacheSolve uses the getinverse() function created in makeCacheMatrix to retrieve the value of the inverse.
#  If it already exists the value is retrieved from the variable set in the parent environment (cached)
#  If it has not been calculated already then the function uses the other functions defined in makeCacheMatrix to
#  first retrieve the value of the matrix, then calculate the value of the inverse, and finally to cache that 
#  value in the parent environment. The function returns the value of the inverse. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv       ## Return a matrix that is the inverse of 'x'
}

######## This is an existing function from the stub file
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
