## makeCacheMatrix and cacheSolve work together to create an environement 
## to store and retrieve the inverse of a matrix

## makeCacheMatrix 
## stores a matrix and a several functions to interact with 
## the matrix in an environment other than the global environment

makeCacheMatrix <- function(x = matrix()) {
      matrixinverse <- NULL
      
      ## fx to store initial matrix in fx environemnt
          set <- function (in.matrix){
            x <<- in.matrix
            matrixinverse <<- NULL
          }
      
      ## fx to retrieve values from initial matrix
          get <- function () x
      
      ## fx to store inverse of matrix in new environment
          setSolve <- function(solve) matrixinverse <<- solve
      
      ## fx to retrieve values from inverted matrix
          getSolve <- function() matrixinverse
      
      ## create list to store values within new environment
          list(set = set,
               get = get,
               setSolve = setSolve,
               getSolve = getSolve
          )
}


## cacheSolve
## retrieves inverse of matrix created with makeCacheMatrix if already solved for
## if not already solved, calculates the inverse of the matrix and stores in fx environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## assign previously solved inverse of matrix
            inverse <- x$getSolve()
        
        ## return if inverse is solved 
            if (!is.null(inverse)){
              print('Getting cached data')
              return(inverse)
            }
        ## if inverse not solved get initial matrix
            data <- x$get()
        
        ## solve for inverse of initial matrix
            inverse <- solve(data, ...)
        
        ## store matrix inverse in fx environment    
            x$setSolve(inverse)
        
        ## return inverse
            inverse
}
