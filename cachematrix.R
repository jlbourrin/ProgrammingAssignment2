## These functions enable to store the value of a matrix and its inverse
## and therefore to check that if a matrix's inverse has already been calculated
## we just retrieve its inverse rather than computing it again

## makeCacheMatrix stores the value of a given matrix and its given inverse (if calculated)
## it also provides 4 child functions: set, get, setinv, and getinv

makeCacheMatrix <- function(x = matrix()) {

      # By default, the value of of the inverse matrix of x is set to NULL
      # but this value would be overriden by setinv() below, thanks to the <<- operator
      # (i.e. assignment from the child function setinv() to its parent environment)
      
      invM <- NULL
      
      # This child function redefines the value of matrix x as y, 
      # and in the process ensures the previous stored inverse matrix is reset to NULL.
      # By setting up InvM to NULL we ensure that InvM will have to be calculated
      # (so if the value of the original matrix has changed, then its inverse will be reset to NULL
      # to be recomputed)
      
      set <- function(y) {
            x <<- y
            invM <<- NULL
      }
      
      # This child function simply retrieves the value of the cache matrix x
      
      get <- function() x
      
      # This child function assigns the inverse matrix of x at the parent level
      # (i.e. store the inverse matrix of x inside makeCacheMatrix's environment)
      
      setinv <- function(inv) invM <<- inv
      
      # This child function simply retrieves the value of the "cache" inverse matrix of x
      
      getinv <- function() invM
      
      # the list of functions below is not necessary for the completion of the task
      # but usefull to easily keep track of the children functions of MakeCacheMatrix
      
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)      
      
}



# This function returns the inverse of the special "matrix" cached in makeCacheMatrix above.
# If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache,
# otherwise it computes it and stores it in the makeCacheMatrix's environment

cacheSolve <- function(x, ...) {

      
      # assign the value of the cache inverse matrix stored in the makeCacheMatrix's environment
      
      inv <- x$getinv()
      
      # tests if "inv" is not NULL
      # if yes, then retrieve the value of the cache inverse 
      
      if( !is.null(inv) ) {x<-matrix
            message("getting cached inverse matrix")
            return(inv)
      }
      
      # Otherwise, gets the cache Matrix from makeCacheMatrix's environment, computes it inverse
      # and stores it in makeCacheMatrix's environment
      
      M <- x$get()
      inv <- solve(M)
      x$setinv(inv)
      # returns the inverse matrix
      inv
      
}
