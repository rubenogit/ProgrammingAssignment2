## These are a pair of functions that cache the inverse of a matrix.

## The function makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {

        s <- NULL

        #function to set value of matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
        }

        #function to get value of matrix
        get <- function() x

        #function to set value of the inverse of the matrix
        setsolve <- function(solve) s <<- solve

        #function to get value of the inverse of the matrix
        getsolve <- function() s

        #make the list of the four defined functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)


}


## The function cacheSolve calculates the inverse of the special "matrix" created with the makeCacheMatrix function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setsolve function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        #get value of the inverse of the matrix
        s <- x$getsolve()

        #if the value of the inverse of the matrix is cached, return that value
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }

        #get the value of the matrix
        data <- x$get()

        #calculate the inverse of the matrix
        s <- solve(data, ...)

        #set (i.e. cache) the value of the inverse of the matrix, then return that value
        x$setsolve(s)
        s
}

#a little test to see it works as expected
#> mym <- matrix(c(3,2,3,4,5,6,3,2,1), nrow=3,ncol=3)
#
#> cachemym <- makeCacheMatrix(mym)
#
#> cacheSolve(cachemym)
#
#           [,1]       [,2] [,3]
#[1,]  0.5000000 -1.0000000  0.5
#[2,] -0.2857143  0.4285714  0.0
#[3,]  0.2142857  0.4285714 -0.5
#
#> cacheSolve(cachemym)
#getting cached data
#           [,1]       [,2] [,3]
#[1,]  0.5000000 -1.0000000  0.5
#[2,] -0.2857143  0.4285714  0.0
#[3,]  0.2142857  0.4285714 -0.5
