## Put comments here that give an overall description of what your
## functions do

## This function will take a square (assumed to be) non-singular matrix as input

makeCacheMatrix <- function(input_mtx = matrix()) {
        ## Initialize a local variable with nulls
        local_mtx <- NULL
        
        ## 
        
        set <- function(free_mtx) {
                local_mtx <<- free_mtx
                local_mtx <<- NULL
        }
        get <- function() input_mtx
        setinv <- function(solve) local_mtx <<- solve
        getinv <- function() local_mtx
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv_mtx <- x$getinv()
        
        if (!is.null(inv_mtx)) {
                message("You have inverted this matrix before. Getting cached matrix.")
                return(inv_mtx)
        }
        message("This is the first time you are inverting this matrix. Using Solve function to calculate inverted matrix")
        data <- x$get()
        inv_mtx <- solve(data, ...)
        x$setinv(inv_mtx)
        inv_mtx
}
