## This function creates a special "matrix" object that 
## can cache its inverse.
## Precondition: matrix should be a square invertible matrix
##
## @param: a matrix object,  
## @return: returns a list object consisting of functions

makeCacheMatrix <- function(x = matrix()) {
        
    		if(!is.matrix(x) | is.null(x)){
    		    x = matrix()
    		}
        
        inverse <- NULL
        
        set <- function(y) {
            if(!is.null(y) & is.matrix(y) & !NA %in% y){
                ## update matrix only if the argument matrix isn't same as
                ## the original matrix object, or if original matrix is empty 
                if(!NA %in% x){
                    if(!areMatricesEqual(x, y)){
                        x <<- y
                        inverse <<- NULL
                        message("New matrix object has been set.")
                    } 
                } else{
                    x <<- y
                }
            } else {
                message("Argument passed is NULL, empty or isn't matrix.")
                message("Please pass valid initialized matrix object.")
            }
        }
        
        get <- function() {x}
        
        setInverse <- function(inv) {
            if(!is.null(inv) & is.matrix(inv)){
                    inverse <<- inv
            }
        }
        
        getInverse <- function() {
            inverse
        }
        
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of a special "matrix" object
## returned by makeCacheMatrix function. If the inverse has already 
## been calculated and the matrix has not changed, then this function 
## retrieve the inverse from cache.The parameter is checked if it is 
## a valid list object returned by makeCacheMatrix
##
## @param: a list object, that is returned by makeCacheMatrix function
## @return: returns the mean of the special matrix object

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      if(isParameterValidObject(x)){
    			invMatrix <- x$getInverse()
    			if(!is.null(invMatrix) ) {
    					message("getting cached data...")
    					return(invMatrix)
    			} 
    			origMatrix <- x$get()
    			if(!NA %in% origMatrix ){
    			    message("Calculating inverse...")
      				invMatrix <- solve(origMatrix, ...)
      				x$setInverse(invMatrix)
      				return(invMatrix)
    			} else {
    			    message("Matrix object of passed parameter is empty.")
              message("First set matrix value using 'set' function.")
    			}
      } else{
          message("Please pass a valid makeCacheMatrix object.")
      }
}

## Utility function that checks if two matrices are equal
## Returns true if the matrices have equal dimensions and all 
## corresponding memeber elements are equal, otherwise false
##
## @param: x and y, the two matrices to be compared
## @return: returns true if they are equal otherwise false

areMatricesEqual <- function(x, y) {
    #message("Comparing Matrix objects ...")
    if(is.matrix(x) & is.matrix(y)){
        if( (ncol(x) == ncol(y)) & (nrow(x) == nrow(y)) ){
            all(x == y)
        } 
    }
    else {
        FALSE
    }
}

## Utility function that checks if an object is a valid makeCacheMatrix object
## That means it shoulb be a list of functions with apprpriate names
## Returns true if it is valid object, otherwise false
##
## @param: param, the list object that will be validated 
## @return: returns true if makeChacheMatrix object otherwise false

isParameterValidObject <- function(param){
    #message("Validating makeChacheMatrix object ...")
    functionNames = c("set", "get", "setInverse", "getInverse")
    !is.null(param) & class(param) == "list" & all(length(param)== 4) & all(names(param)==functionNames)
}
