## Caching the Inverse of a Matrix

## makeCacheMatrix creates a special "matrix" object
## the makeCacheMatrix also contains a list of functions to:
## set the matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     ## makes sure the inverse is empty and clean
     inv <- NULL
     
     ## the set function recieves a matrix as argument : y
     ## assigns the matrix to x which is defined in the 
     ## parent function makeCacheMatrix
     ## and it again makes sure that the inverse isn't computed
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     
     ## get functions returns the value of special matrix defined via set
     get <- function() x
     
     ## setinverse sets the inv value through 'inverse' (type:matrix) argument
     setinverse <- function(inverse) inv <<- inverse
     
     ## getinverse retrieves the value of the inv matrix
     getinverse <- function() inv
     
     ## makeCacheMatrix returns a list
     ## containing the functions defined above
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

##=================== [cacheSolve] ============================##


## cacheSolve computes and returns the inverse of the special "matrix" :x 
## set by makeCacheMatrix's set function. If the inverse has already been 
## calculated (and the matrix has not changed), then the cacheSolve function 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {

     ## retrieves the previous stored value of inverse matrix
     ## this could be a NULL value or a previously computed inverted matrix
     inv <- x$getinverse()
     
     ## if the inverse was computed previously
     ## display a message to the user and return the computed matrix
     ## and exit the function (don't continue)
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     
     ## otherwise retrieve the original special matrix and save it in data
     data <- x$get()
     
     ## compute the invert of 'data' and store it in inv
     inv <- solve(data, ...)
     
     ## save the new inverted matrix into cache by calling the setinverse
     ## function defined in makeCacheMatrix
     ## and display the output using the extracting ($) sign 
     x$setinverse(inv)
}
