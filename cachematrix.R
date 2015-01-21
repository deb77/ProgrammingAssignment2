## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##function makeCacheMatrix creates a special "vector", which is really a list containing a function to
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the matrix inverse
##4. get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(inv) inverse <- inv
	getInverse <- function() inverse
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## Write a short comment describing this function
#The following function calculates the inverse of a matrix.
#However, it first checks to see if the matrix inverse has 
#already been calculated. If so, it gets the inverse matrix 
#from the cache and skips the computation. 
#Otherwise, it calculates the matrix inversion of the data and 
#sets the value of the matrix inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                return(m)
        }
        data <- x$get()
	  if(nrow(data)==ncol(data) && det(data)!=0)
	  {
       	 m <- solve(data, ...)
       	 x$setInverse(m)
        	 return(m)
	  }
	  return(NULL)

}
