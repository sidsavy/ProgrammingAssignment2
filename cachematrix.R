
##Function calculates the inverse of the matrix
## If the matrix inverse has already been calculated it will instead and find in the cache and return it

makeCacheMatrix <- function(x = matrix()) 
{
     m <- NULL
      set <- function(y) 
      {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The function cacheSolve returns the inverse of a matrix A created with a makecachematrix function
## If the cached inverse is available ,cacheSolve retrieves it, while ifnot, it computes, caches, and returns it.

cacheSolve <- function(x, ...) 
{
## Return a matrix that is the inverse of 'x'
   m <- x$getinverse()
      if(!is.null(m)) 
      {
            message("getting cached data")
            return(m)
      }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m

}
         
         
         
        

