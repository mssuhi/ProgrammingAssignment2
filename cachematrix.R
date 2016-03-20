## This function creates a special "matrix" object 
## that can cache its inverse
## The functions can get or set a matrix and get or set its inverse 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
     ## If found in cache then return from the cached value
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
    ##If not found in the cache then find the inverse and set the cache value
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

## results of the above code
##> x = rbind(c(2, 3), c(1, 2))
##> x
##[,1] [,2]
##[1,]    2    3
##[2,]    1    2
##> i = makeCacheMatrix(x)
##> cacheSolve(i)
##[,1] [,2]
##[1,]    2   -3
##[2,]   -1    2
##> cacheSolve(i)
##getting cached data
##[,1] [,2]
##[1,]    2   -3
##[2,]   -1    2

##> x = rbind(c(1, -1/4), c(-1/4, 1))
##> i = makeCacheMatrix(x)
##> cacheSolve(i)
##[,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667
##> 
 


