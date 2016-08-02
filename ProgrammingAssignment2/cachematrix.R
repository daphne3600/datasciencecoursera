## cachematrix will calculate the inverse of a matrix and use cache
## to avoid calculation if it exists

## makeCacheMatrix will:
## Set the value of the matrix
## Get the value of the matrix
## Set the value of the inverse
## Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y ##Set new values of x
    i <<- NULL ##Clear cache of old calculated inverse
  }
  get <- function() x ##Get new values of x
  setinverse <- function(inverse) i <<- inverse ##Set the values of i
  getinverse <- function() i ##Get the values of i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## Create a list of named functions
}

## cacheSolve will:
## Return a matrix that is the inverse of x

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) { ##Check if inverse was already calculated
    message("getting cached data")
    return (i)
  }
  data <- x$get() ##Get the value of x
  i <- solve(data) ##Calculate inverse of x
  x$setinverse(i) ##Set the value of i
  i ##Print
}
