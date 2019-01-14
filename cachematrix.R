## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function accepts matrix value, caches it and inverses it.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve checks if the matrix is already cached and inverses if necessary.

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inversed data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

B <- matrix(c(1,2,3,4),2,2)
B1 <- makeCacheMatrix(B)
print(cacheSolve(B1))
print(cacheSolve(B1))
