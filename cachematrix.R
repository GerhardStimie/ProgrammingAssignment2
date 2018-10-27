## Assignment 2 submission
##
##      This source file contains 2 functions that:
##              1. Create a list of functions to calculate the inverse of a passed matrix
##              2. Store the resulting inverse matrix in cache
##              3. Return the cached matrix upon request if the inverse has been calculated before

## There are 2 functions in this source file:
##
##      makeCachematrix
##
##              accepts an optional source matrix ("input_mtx") to be inverted as input. 
##              as input. 
##              outputs a list of functions as follows:
##                      set     Used to set the matrix to be converted. If this function
##                              is called directly, it will overwrite any matrix that
##                              was passed in the original call to makeCachematrix
##                      get     When called this function (that takes no input)
##                              returns the matrix to be inverted as stored in the 
##                              "input_mtx" parameter (either as it was passed in
##                              the original call to "makeCachematrix" or as overwritten
##                              by the matrix passed in "new_mtx"in a call to the 
##                              "set" function)
##                      setinv  Calculates the inverse of the "input_mtx" matrix and 
##                              caches the resultin matrix in "cache_mtx"
##                      getinv  Returns the matrix currently stored in "cache_mtx"
##
##      cacheSolve
##
##              accepts a list vector previously created by a call to the "makeCacheMatrix"
##              function and returns the inverse of the matrix indicated in that list from
##              either cache (if it has been calculated before), or by calculating it using
##              the solve function if it was not calculated before. If the inverse matrix has not
##              been calculated before, it is cached for future retrieval.
##      

#############   HOW TO TEST THESE FUNCTIONS   #################################################
##
##      1.      Create a square non-singular input matrix, eg mtx_1 <- matrix(rnorm(9,1), 3, 3)
##      2.      Create a list using makeCacheMatrix, eg list_1 <- makeCacheMatrix(mtx_1)
##      3.      Calculate the inverse using function cacheSolve, eg inv_1 <- cacheSolve(list_1)
##      4.      Note the message to indicate if the inverse is calculated anew
##      5.      Rerun the function for the same list, eg inv_2 <- cacheSolve(list_1)
##      6.      Again note the message indicating that the inverse was returned from cache
##      7.      comparing the the resulting inverted matrices using "identical" function should
##              in a "TRUE" result, eg identical(inv_1, inv_2)
##      8.      To test that the inverse is correct, multiplying (using matrix multiplication)
##              the original matrix with the inverse matrix should produce an identity matrix,
##              (round the result to see it more clearly) eg round(solve(mtx_1 %*% inv_1))
##
##############################################################################################

makeCacheMatrix <- function(input_mtx = matrix()) {
       
        cache_mtx <- NULL
        
        set <- function(new_mtx) {
                input_mtx <<- new_mtx
                cache_mtx <<- NULL
        }
        
        get <- function() input_mtx
        
        setinv <- function(solve) cache_mtx <<- solve
        
        getinv <- function() cache_mtx
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## This function returns the invered matrix for the matrix of a matrix through the 
## "input_mtx" passed the the function makeCacheMatrix through a direct call or
## passed through the "new_mtx" parameter of the "set" function from a list previously  
## created by a call to the makeCacheMatrix function.


cacheSolve <- function(input_list, ...) {

        ## load the cached inverted matrix for this list. if it exists, return the cached 
        ## inverted matrix. If not, use solve function to calclulate the inverted matrix
        ## and cache the result for this list.
        
        inv_mtx <- input_list$getinv() 
        if (!is.null(inv_mtx)) {
                message("You have inverted this matrix before. Getting cached matrix.")
                return(inv_mtx)
        }
        message("This is the first time you are inverting this matrix. Using Solve function to calculate inverted matrix")
        mtx <- input_list$get()
        inv_mtx <- solve(mtx, ...)
        input_list$setinv(inv_mtx)
        inv_mtx
}