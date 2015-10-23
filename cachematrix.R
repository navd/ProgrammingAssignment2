## Creates a method for creating matrix and do caching of its inverse

makeCacheMatrix <- function(x = matrix()) {
           inv <- NULL       	## Initialize the inverse variable
           set <- function( matrix ) {##setting the matrix
                      m <<- matrix
                      inv <<- NULL
              }
              get <- function()	m ## Method to get the matrix
              setInverse <- function(inverse) inv <<- inverse ## Method to set the inverse of the matrix
              getInverse <- function()  inv     ## Method to get the inverse of the matrix
              list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)## Return a list of the methods
        }

## Method to compute inverse of the special matrix returned by "makeCacheMatrix" and return inverse if already present

cacheSolve <- function(x, ...) {
        m <- x$getInverse()         ## Get the inverse of matrix
        if( !is.null(m) )           ## return the inverse if its already set
          {
                message("getting cached data")
                return(m)
           }
          data <- x$get()## Get the matrix 
          m <- solve(data) %*% data ## Calculate the inverse 
          x$setInverse(m) ## Set the inverse 
          m  ## Return the matrix
}
