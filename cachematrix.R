## The function below is used to cache the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                 # initializing inv as NULL
  set <- function(z){
    x <<- z
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) { inv <<- inverse}
  getInverse <- function() {inv}             
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The function below is used to check if inverse is already calculated
## if so, it returns the cached matrix for faster computations
## if not, inverse is calculated and returned

cacheSolve <- function(x, ...) {          #gets cache data
  inv <- x$getInverse()
  if(!is.null(inv)){                      # checking whether inverse is null
    message("getting cached data")
    return(inv)                           # returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat,...)                   # calculates inverse value
  x$setInverse(inv)
  inv                        ## Return a matrix that is the inverse of 'x'
}
