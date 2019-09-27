## Programming Assignment 2
## Write a pair of functions that cache the inverse of a matrix
## These two functions are "makeCacheMatrix" and "cacheSolve"
## The "makeCacheMatrix" function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        set_inv_mat <- function(mat) inv_mat <<- mat
        get_inv_mat <- function() inv_mat
        list(set = set, get = get,
             set_inv_mat = set_inv_mat,
             get_inv_mat = get_inv_mat)
}


## The "cacheSolve" function computes the "matrix" returned by the "makeCacheMatrix" function
## If the inverse has already been calculated (and the matrix has not change),
## then the cacheSolve function should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        inv_mat <- x$get_inv_mat()
        if(!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        data <- x$get()
        inv_mat <- solve(data, ...)
        x$set_inv_mat(inv_mat)
        inv_mat
        ## Return a matrix that is the inverse of 'x'
}
