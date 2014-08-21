## Inverting matrices is a computational intensive task, these two functions 
## cashes the inverse of a matrix and if the matrix is unchanged returns the 
## cached inverted matrix

## This function will take a matrix as argument and returns a list of functions
## accessing the matrix and the inverse of the matrix stored in the local 
## environment

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL       # sets the inverse to NULL in the function environment
  
    # creates a function that takes a matrix as argument and stores it in the 
    # makeCacheMatrix environment (using the <<- operator) it also resets the 
    # inverse to NULL
    set <- function(y) {     
        x <<- y 
        inverse <<- NULL
    }
  
    #creates a function that returns the matrix. Since x is not defined in the 
    # function R will find it in the calling environment
    get <- function() x 
  
    # creates a function that sets the inverse variable in the makeCacheMatrix 
    # environment
    setinverse <- function(i) inverse <<- i

    # creates a function that returns the inverse varible defined in the 
    # makeCacheMatrix environment
    getinverse <- function() inverse
  
    # creates and returns a list of the functions created above with the same
    # names in the list.
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## function that takes a makeCacheMatrix as argument and returns the inverse
## of the matrix. It will also cache the matrix and return the cached 
## matrix next time the function gets called provided that the matrix
## hasnt changed.

cacheSolve <- function(x, ...) {

    inverse <- x$getinverse()   # reads the inverse from x
  
    # if the inverse is not null return the cached inverse
    if(!is.null(inverse)) {     
        return(inverse)
    }
  
    # if the inverse is null because it hasn't been calculated yet or
    # it has been reset to null by the $set function the inverse has
    # to be calculated.
  
    data <- x$get()              # get the matrix from x
  
    # calculate the inverse of matrix (according to the instructions
    # it is ok to assume that the matrix is invertible)
    inverse <- solve(data, ...)  
    x$setinverse(inverse)        # set the inverse variable in x
    inverse                      # return the inverse
}
