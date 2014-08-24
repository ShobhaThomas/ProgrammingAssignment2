##makeCacheMatrix is a  function which creates a special "matrix" object 
##that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##cacheSolve is a function which computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed)
##then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
## Test run:-
## > x = rbind(c(1, 1/2), c(1/2, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##[,1] [,2]
##[1,]  1.0  0.5
##[2,]  0.5  1.0

## No cache in the first run
## > cacheSolve(m)
##[,1]       [,2]
##[1,]  1.3333333 -0.6666667
##[2,] -0.6666667  1.3333333


## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
## [,1] [,2]
## [1,]  1.3333333 -0.6666667
##[2,] -0.6666667  1.3333333
