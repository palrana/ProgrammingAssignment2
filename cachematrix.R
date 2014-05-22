
# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
   
   # inv stores the cached value of inverse matrix
   inv <- NULL
   
   # sets the matrix
   set <- function(y) {
      x <<- y
      inv <<- NULL
    }
   
   # get the matrix
   get <- function() x
   
   # set the inverse of the matrix
   setInverse <- function(inverse) inv <<- inverse
   
   # get the inverse of the matrix
   getInverse <- function() inv
   
   # returns the list of function
   list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.

cacheSolve <- function(x, ...) {
  
    inv <- x$getInverse()
    # If inverse is already calulated then return the cached inverse
        
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
  
    # If inverse not calculated or if it is null then calculate the inverse
    data <- x$get()
    inv <- solve(data)
  
    # Set the calculated inverse in cache
    x$setInverse(inv)
  
    # return the inverse
    inv
  
}
