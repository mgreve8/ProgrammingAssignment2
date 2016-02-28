## These functions help to cache the inverse of a matrix and
## return that inverse if needed
##
## To get the inverse returned to the console
## You need to first source this file and then
## input an invertable matrix into the makeCacheMatrix function
##
## Example
## > test_var <- matrix(c(rnorm(4)),2,2)
## > new_var <- makeCacheMatrix(test_var)
## > cacheSolve(new_var)
##            [,1]        [,2]
## [1,] 0.06356938  0.53502221
## [2,] 0.62917222 -0.07205241

## The following function creates a special matrix, which basically
## is a list containing a function to 
##
## 1. Set the value of the matrix
## 2. Get the value of the matirx
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse Matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inv <- function(inv) m <<- inv
  get_inv <- function() m
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

## The following function actually calculates the inverse of the 
## special matrix you created with the above function and will 
## return cached data if it is already there or will actually
## solve for the inverse matrix

cacheSolve <- function(x, ...) {
  m <- x$get_inv()
  if(!is.null(m)) {
    message("Acessing Cached Data")
    return(m)
  }
  z <- x$get()
  m <- solve(z,...)
  x$set_inv(m)
  m
}
