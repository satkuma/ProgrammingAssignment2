## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix function stores the cache of the inverse of the matrix. The actual inverse is not calculated in this function. It only stores
# and retrieves the data from the cache.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#cachSolve function checks whether the cache already has data. if the data is not available, then it calculates the inverse
# using solve() function and returns the inverse of the matrix. In addition, it sets the cache with the inverse of the matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## Sample run of the above functions below
## The code below is a sample run. 
## matrix1 is a invertable matrix. As per the assumption of the assignment, the matrix can be assumed invertible.
matrix1 <- matrix(1:4, nrow = 2, ncol = 2);
# value stores the matrix mentioned above. Still inverse of matrix1 is not calculated.
value <- makeCacheMatrix(matrix1);
# cacheSolve(value) calculates the inverse and caches in makeCacheMatrix.
cacheSolve(value)

