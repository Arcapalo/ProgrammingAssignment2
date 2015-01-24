## This function fix some objects in the memory
## that can be reused.

makeCacheMatrix <- function(x = matrix()) {
    ## sets x equal to an empty matrix
    ## Set the inverse equal to NULL
    I <- NULL
    set <- function(y){
      ## set function assigns the argument to x
      x <<- y
      ## Once the set function is called, Inverse is re-set to NULL
      I <<- NULL
    }
    ## get function returns the matrix
    get <- function() x
    
    ## setInverse overrides the previous value of I and assigns the argument to Inverse (which is
    ## supposed to be the inverse of matrix x)
    setInverse <- function(solve) I <<- solve
    
    ## getInverse returns the Inverse
    getInverse <- function() I
    
    ## creates a list of the functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function returns the inverse matrix of the
## provided in makeCacheMatrix
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("Returning INVERSE already in cache")
    return(inverse)
  }
  message("Calculating INVERSE . . .")
  data <- x$get()
  ##I <- solve(data, ...)
  inverse <- tryCatch(solve(data, ...),error=function(e) {print(paste("M is singular, cannot be inverted."))})
  x$setInverse(inverse)
  inverse
}
