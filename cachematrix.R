## Put comments here that give an overall description of what your
## functions do

## This function creates a list to stores functions to 
## set & get the matrix, and set & get the value of the inverse of 
## this matrix

makeCacheMatrix <- function (x = matrix()){
      inverse <- NULL
      set <- function(y){
            x <<- y
            inverse <- NULL
      }
      
      get <- function () x
      
      setInverse <- function(inv) inverse <<- inv
      
      
      getInverse <- function() inverse
      
      list(set = set, get = get, setInverse = setInverse, 
           getInverse = getInverse)
}

## this function checks if the inverse have already been calulated.
## If not, it calcule the inverse using the Solve function.
## If yes, it shows the invers and the message "Getting cached data!"


cacheSolve <- function(x, ...){ 
      ## Return a matrix that is the inverse of 'x'
      inverse <- x$getInverse()
      if(!is.null(inverse)){
            message("Getting cached data!")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data)
      x$setInverse(inverse)
      inverse
      
} 