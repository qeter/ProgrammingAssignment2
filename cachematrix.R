## Programming Assignment 2 


## 1
# This function makes a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL 
      }
      get <- function() x
      setInverse <- function(solveMatrix) inv <<- solveMatrix
      getInverse <- function() inv
      list(set = set, 
           get = get, 
           setInverse = setInverse, 
           getInverse = getInverse)
}

#####

## 2
# First this function checks if there is already an invertion of the given matrix 
# If this is the case, it returns this value
# If not, it solves the matrix invertion and stores it in the cache for later use 
cacheSolve <- function(x, ...) {
      # Check if there is already a cached invertion
      # If this is the case, get the invertion and terminate the function
      inv <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      # If no invertion is already in cache, calculate the invertion
      data <- x$get()
      inv <- solve(data)
      # Save the invertion for later use
      x$setInverse(inv)
      # Return the invertion
      inv      
}
