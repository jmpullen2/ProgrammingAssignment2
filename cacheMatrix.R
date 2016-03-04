# R Programming Assignment 2
# Demonstration of cacheing in context of matrix inverse

# makeCacheMatrix: This function creates a special "matrix" object that can cache its 
# inverse. We assume that the matrix supplied is always invertible.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve

        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}



# testCahce: function to test matrix cache functions
testCache <- function() {

  # make a 2x2 diagonal matrix and print it
  z = matrix(nrow=2,ncol=2)
  z[1,1]=2;z[1,2]=0;z[2,1]=0;z[2,2]=2
  print("test matrix:")
  print(z)

  # make its cache
  y<-makeCacheMatrix(z)

  # cache the inverse
  print("cached inverse of test matrix:")
  cacheSolve(y)
}

