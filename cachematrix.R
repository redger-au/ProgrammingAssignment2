## http://adv-r.had.co.nz/Functional-programming.html
##
## A set of 2 functions to find the inverse of a matrix
## INPUT DATA MUST BE AN INVERTIBLE MATRIX (ie. square and orthogonal)
##     Note that the functions are optimised for repeated use
#      the aim is to only invert each matrix once ie. re-use existing data for matrices already processed
#
# ---------------------------------------------- #
## makeCacheMatrix creates a cache of already processed matrices - to avoid reprocessing
##    This function is to be called by the user, passing the original matrix
##      for which the inverse will subsequently be sought
##    Obtaining the inverse is NOT usually performed directly by this function
##      call via cacheInv function instead
makeMatrix <- function(x = matrix()) {
    inv <- NULL
    # "set" function ie. set the value for a new matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # Get the value for a matrix already processed
    get <- function() x
    # Set the inverse of the existing matrix
    set_inv <- function(solve) inv <<- solve # Solve produces the inverse
    # Retrieve the stored inverse or a previously processed matrix
    get_inv <- function() inv # Temporarily
    # Create the list of functions used by callers
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}
## The function to be called by users wishing to calculate the inverse of a matrix
##
## Prior to calling this function you must have established the basics
##   Create the necessary global settings by calling makeMatrix with your initial matrix, and "keep" the returned object
##        eg. mkM <- makeMatrix(matrix(runif(4,2,2)))
##   Then call the following to obtain the inverse, passing "mkV" as the parameter
##        ie. answer_inv <- cacheInv (mkM)
##   Assumes that the initial matrix is square and orhtogonal and thus invertible
##
cacheInv <- function(mkMtrx_in, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- mkMtrx_in$get_inv()
    # If we have already created the invese and saved it, return that
    if(!is.null(inv) && !is.na(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- mkMtrx_in$get()
    # Create the inverse for a "new" matrix
    inv <- solve(data, ...)
    # Save the newly calculated inverse for subsequent re-use
    mkMtrx_in$set_inv(inv)
    inv
}
