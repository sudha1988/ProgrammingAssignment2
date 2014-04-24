##################################################################
## makeCacheMatrix can be be used for managing the setting      ##
## getting operation of input matrix and cached inverse matrix, ##
## Like,  storing the inverse matrix and input matrix globally. ##
##################################################################

makeCacheMatrix <- function(inputMatrix = matrix()) {
  ## needed, otherwise getInverse will produce error 'inversedMatrix not defined' if we call getInverse directly.
  inversedMatrix <<- NULL   

  ## to initialize inputMatrix
  set <- function(input) {    
    inputMatrix <<- input
    inversedMatrix <<- NULL
  }
  
  ## to fetch the inputmatrix
  get <- function() {
    inputMatrix
  }
  
  ## to set the inversed result to the global env
  setInverse <- function(inv = matrix()) {
    inversedMatrix <<- inv
  }
  ## to get the cached inverse result
  getInverse <- function() {
    inversedMatrix
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##############################################################################
## cacheSolve function will use the cached inverse. If its null, It will    ##
## compute the inverse matrix.                                              ##       
##############################################################################
cacheSolve <- function(functionList, ...) {
  
  inverse <- functionList$getInverse()
  
  ## Check if its cached. If yes return the cached data
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## If cache is empty, do the following steps.
  ## 1.get the input matrix
  
  data <- functionList$get()
  
  ## 2.compute the inverse matrix
  inverse <- solve(data)
  
  ## 3.set the cache with the result returned by step 2.  
  functionList$setInverse(inverse)
  
  inverse

}
