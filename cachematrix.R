## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     
     invmtx <- NULL # store the result of inversion here into invmtx
     set <- function(y) {
          x <<- y
          invmtx <<- NULL # initialize invmtx and assign it a value NULL
          # in an environment that differs from the current
     }
     get <- function() x # return the input matrix
     setinvrs <- function(inverse) invmtx <<- inverse # set the inversed matrix
     getinvrs <- function() invmtx # return the inversed matrix
     
     # return a list that contains the functions defined above
     list(set=set, get=get, 
          setinvrs=setinvrs, 
          getinvrs=getinvrs)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     invmtx <- x$getinvrs() # get the inversed matrix from object x
     if(!is.null(invmtx)) {  # check if invmtx has already been calculated 
          # (invmtx is not NULL, as initialized, if calculated)  
          message("getting cached data")
          return(invmtx) # if invmtx has already been calculated, return it, otherwise...
     }
     mtx <- x$get() # get the input matrix 
     invmtx <- solve(mtx) # calculate the inverse of the input matrix
     x$setinvrs(invmtx) # set the inverse matrix as object
     invmtx # returns the inverse matrix
}
