# Mei O'Halloran
# Coursera Data Science Specialization
# R-Programming
# Assignment 2 -  Lexical Scoping - 

## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that
## stores a matrix and caches its inverse.
## This function creates a special "matrix" object that can cache its inverse.

## cachematrix.R - Caching the Inverse of a Matrix.
##
## This program consists of pair of functions that cache the inverse of a matrix.
##
## 1. makeCacheMatrix: This function creates a special "matrix" object that
##    can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix"
##    returned by makeCacheMatrix above. If the inverse has already been calculated,
##    then the cacheSolve will retrieve the inverse from the cache.

## The following function "makeCacheMatrix" creates a special "matrix", which
## is a list containing a function to
##
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix
##############################################################################
# this function works like a class, it creates a list
# of four functions: set, get, setInv and getInv. 
# it uses <<- assignment operator so that
# these internal variables are not exposed to the
# outside environment. 

# Example usage:
# > x <- matrix(rnorm(16), nrow = 4)          // Create a matrix x
# > cx <- makeCacheMatrix(x)                  // Create our special matrix
# > cx$get()                                  // Return the matrix
# > cacheSolve(cx)                            // Return the inverse
# > cacheSolve(cx)                            // Call the 2nd time, so return
#                                             // the cached inverse


# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
##############################################################################3
makeCacheMatrix <- function(x = matrix()) {
     # inv will store the cached inverse matrix
     inv <- NULL
     
     # Setter for the matrix
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     # Getter for the matrix
     get <- function() x
     
     # Setter for the inverse
     setinv <- function(inverse) inv <<- inverse
     # Getter for the inverse
     getinv <- function() inv
     
     # Return the matrix with our newly defined functions
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##############################################################################
# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
##############################################################################
cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     
     # If the inverse is already calculated, return it
     if (!is.null(inv)) {
          message("getting cached data ...  matrix inverse")
          return(inv)
     }
     
     # The inverse is not yet calculated, so we calculate it
     data <- x$get()
     inv <- solve(data, ...)
     
     # Cache the inverse
     x$setinv(inv)
     
     # Return it
     inv
}

## TEST FUNCTIONS with the runtest function
runtest <- function(x, ...) {
x <- matrix(rnorm(16), nrow = 4)          ## Create a matrix x
print (x)
cx <- makeCacheMatrix(x)                  ## Create our special matrix
cx$get()                                  ## Return the matrix
cacheSolve(cx)                            ## Return the inverse
cacheSolve(cx)                            ## Call the 2nd time,
#                                            ## cached inverse
}
