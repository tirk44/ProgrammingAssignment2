## These functions calculate the inverse of a stored matrix in an efficient manner

## This function makeCacheMatrix stores a matrix to be solved
## and calculates then caches the inverse of the matrix for future use

makeCacheMatrix <- function(x = matrix())
  {  ##this section clears any memory from previous runs of the function
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }  ##this section populates a list with the matrix and its inverse and null values
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function checks for the cached value of the inverse matrix
## if it is not cached, it calculates

cacheSolve <- function(x, ...) {
        ## check for cached data
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()  ##gets the matrix from previous function
  m <- solve(data, ...) ##calculates the inverse
  x$setinv(m)
  m ##displays the solution, the inverse matrix of the stored matrix from make cache matrix
)