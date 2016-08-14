#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
#The following functions makeCacheMatrix and cacheSolve are used to cache the inverse of a matrix.
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #inverse variable
  set<-function(y)
  {
    x <<- y #assign value to x diff from x in current env
    inv <<-NULL
  }
  
  get<-function() 
  { 
    x #return value
  }
  
  setinverse<-function(inverse)
  {
    inv <<- inverse
  }
    
  getinverse<-function()
  {
    inv #return value
  }

  list(set = set, get = get, getinverse = getinverse, getinverse= getinverse)

}


#The following function calculates the inverse of the special "vector" created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <-x$getinverse()
    if (!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv <-solve(data)
    x$setinverse(inv) 
    inv
}
