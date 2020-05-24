##this program contains two functions to cache the inverse of a matrix. The matrix inverse is calculated using solve() 

## Function 1 creates a special matrix which is a list that 1. Sets the value of the matrix 2. Gets the value of the matrix 
## 3. Sets the inverse of the matrix and 4. Gets the value of the inverse of the matrix


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


## The following function calculates the inverse of the special "matrix". It first checks to see if the
## inverse has already been calculated. If so, it `get`s the mean from thecache. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
