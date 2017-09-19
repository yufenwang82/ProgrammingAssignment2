## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

## we use the layout of the makeVector function to write the makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
 inverse_matrix<-NULL
 set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
    }
 get <- function() x
 set_inverse <- function(solveMatrix) inverse_matrix <<- solveMatrix
 get_inverse <- function() inverse_matrix
 list(set = set, get = get,
      set_inverse= set_inverse,
      get_inverse = get_inverse)

}


## The cacheSolve function  computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse_matrix <- x$get_inverse()
    if(!is.null(inverse_matrix)) {
        message("getting cached data")
        return(inverse_matrix)
    }
    data <- x$get()
    inverse_matrix <- solve(data, ...)
    x$set_inverse(inverse_matrix)
    inverse_matrix
}
