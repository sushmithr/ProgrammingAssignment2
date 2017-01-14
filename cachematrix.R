## Both these functions are used for the general purpose of creating a matrix 
## that has a list of functions that can be called to simply "get" the matrix
## values, "set" the values of the matrix, "setsol" or set the inverse of the matrix,
## and "getsol" or get the inverse of the matrix. The first use this function
## to create a matrix and then get the inverse of the matrix using "getsol" will result in a
## return value of NULL, as we have not calculated the inverse yet. This can be done with the
## cacheSolve function, which takes the original matrix we created as an input and uses
## the solve function to get the inverse and uses the "setsol" function to set the previously
## NULL inverse to whatever was calculated. If we need to call "cacheSolve" again to get the
## inverse of the matrix, then the function will simply call the "getsol" function of the 
## matrix assuming it was not changed, which saves time over calling the solve function
## repeatedly.

## This function will create a matrix of values based on the parameters passed by the user. 
## For instance you might use "x <- makeCacheMatrix(matrix(1:6, 3, 2))" to create a matrix
## containg values 1-6 and store that in the variable x. Then you couls use "x$get" to have 
## the matrix outputted and displayed in the console. The other functions include "set", which 
## sets the matrix to different values if needed, "setsol", which is used to set the value of 
## inverse of teh matrix, and "getsol", which is used to output and display the inverse of the
## current matrix.

makeCacheMatrix <- function(x = matrix()) {
		s <- NULL
        set <- function(y) 
        {
            x <<- y
            s <<- NULL
        }
        get <- function() x
        setsol <- function(sol) m <<- sol
        getsol <- function() s
        list(set = set, get = get,
             setsol = setsol,
             getsol = getsol)
}


## This function takes a matrix as input and calcultes the inverse of it. This is done using the
## solve function, however this is not always the case. For instance, if the matrix remains the 
## same (this can be checked by finding out whether the "getsol" function of the matrix is NULL, which
## implies that inverse of the matrix had not been calculated previously) and we end up calling the
## cacheSolve function again, rather than calling the solve function once again, the function will 
## simply use the "getsol" function to return the already calculated inverse. Otherwise it will go
## on to use the "setsol" to set the inverse of the matrix based on what was calculated using the solve
## function.

cacheSolve <- function(x, ...) {
        s <- x$getsol()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsol(s)
        s
}
