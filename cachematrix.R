## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ptm <- proc.time()
      # from Cache, get Matrix "m" 
      m <- x$getinv()             
      # Check if it's null, Retrun value which is read from Cache
      if(!is.null(m)) {
        message("retrieving cached data")
        cat("Process Time : ", proc.time() - ptm)
        return(m)
      }
      # if "m" == NULL, get original Matirx
      mydata <- x$get()
      
      # Calculate inverse using Solve
      m <- solve(mydata, ...)
      x$setinv(m)
      cat("Process Time", proc.time() - ptm)
      # Returm inverse
      m
}
