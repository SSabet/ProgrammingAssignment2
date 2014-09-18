## This script contains two functions which help us to store an invertible matrix,
## compute its inverse, and cache the inverse matrix for further use.

## The first function creates a special kind of matrix, equipped with 4 functions 
## which can be used to set/get the value of this matrix, and set/get the value 
## of its inverse, along with a cache to store the inverse matrix. It returns 
## back a list containing the 4 aforementioned functions.


makeCacheMatrix <- function(mat = matrix()) {
      
      # Define a variable as the cache
      
      Inv <- NULL
      set <- function(newMat){
            mat <<- newMat
            Inv <<- NULL
      }
      
      get <- function() mat
      setInv <- function(Inverse) Inv <<- Inverse
      getInv <- function() Inv
      
      list(set=set, get=get, setInv=setInv, getInv = getInv)
}


## The function cacheSolve returns the inverse of a matrix x which has been 
## created by the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {

      ## It first tries to use an already computed and cached inverse matrix, 
      ## in case such a cached result is available.
      
      Inv <- x$getInv()
      if (!is.null(Inv)){
            message("getting cached inverse")
            return(Inv)
      }
      
      ## Otherwise it computes the inverse matrix and caches and return the result
      
      origMat <- x$get()
      Inv <- solve(origMat,...)
      x$setInv(Inv)
      
      Inv
      
}
