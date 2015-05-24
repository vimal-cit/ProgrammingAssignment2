## Function that calculates & caches the inverse of the Matrix

## Sample Usage:
## Source the Code: source ("cachematrix.R")
## Create a sample matrix: m1<-makeCacheMatrix(matrix(c(3,0,0,3),c(2,2)))
## Call cacheSolve function to get the Inverse: cacheSolve(m1)
## Below is the result:
##           [,1]      [,2]
## [1,] 0.3333333 0.0000000
## [2,] 0.0000000 0.3333333
## Create a second sample matrix: m2<-makeCacheMatrix(matrix(c(5,0,5,5),c(2,2)))
## Call cacheSolve function to get the Inverse: cacheSolve(m2)
## Below is the result:
## [,1] [,2]
## [1,]  0.2 -0.2
## [2,]  0.0  0.2
## Calling cacheSolve (m1) again, will fetch the results from the cache
## > cacheSolve(m1)
## Getting Cached Data
## [,1]      [,2]
## [1,] 0.3333333 0.0000000
## [2,] 0.0000000 0.3333333
## Calling cacheSolve (m2) again, will fetch the results from the cache
## > cacheSolve(m2)
## Getting Cached Data
## [,1] [,2]
## [1,]  0.2 -0.2
## [2,]  0.0  0.2


## Create a special Matrix which is a list along with the functions to peform
## Set the Value of the Matrix
## Get the Value of the Matrix
## Set the Value of the Inverse Matrix
## Get the Value of the Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function (y) {
    x <<- y
    i <<- NULL
  }
  get <- function () x
  setinverse <- function (inv) i <<- inv
  getinverse <- function() i
  
  list (
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
  
}


## Calculate the Inverse of the Matrix created with the above function, returning the results
## if it is already available in Cache, else Calculate the inverse and store in the Cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("Getting Cached Data")
    return(i)
  }
  m <-x$get()
  i<-solve(m,...)
  x$setinverse(i)
  i
}
