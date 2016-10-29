## This pair of functions will allow you to cache an inverse 
## of a square matrix

## This function will create a special matrix object that can cache its invers

makeCacheMatrix <- function(x = matrix()) {

    inversem <- NULL
    set <- function(y){
        x <<- y
        inversem <<- NULL
    }
    
    get <- function() x
    setinv <- function(solve) x<<-solve
    getinv <- function() inversem
    list(set=set,get=get,setinv=setinv,getinv=getinv)
    
}


## this function calculates the inverse of the matrix returned 
##  if the inverse has already been calculated, it uses the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    inversem <- x$getinv()
    print (inversem)
    if(!is.null(inversem)){
        message ("getting cached data")
        return(inversem)
  }
    
    data <- x$get()
    
    inversem <- solve(data, ...)
    
    x$setinv(inversem)
    inversem
}
