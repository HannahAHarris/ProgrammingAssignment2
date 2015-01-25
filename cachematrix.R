## The two functions in this project ('makeCacheMatrix' and 'cacheSolve') work together. The functions themselves build heavily on the example provided in the 'readme.md' section of the assignment2 file.
## As noted in the example, the <<- operator assigns a value to an object in an environment that is different from the current environment.
## The goal of the two functions below is to cach the inverse of a matrix.

## The 'MakeCacheMatrix' function creates a special 'matrix' that is actually a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## 'cacheSolve' calculates the inverse of the special 'matrix' created with the 'makeCacheMatrix' function.
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache (noting this with a string "getting cached data" before the output) and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
