## Week 3 Programming Assignment - JHU Data Science 3/13/2016

## This function creates a special 'matrix' so that its inverse can be 
## calculated and cached.

makeCacheMatrix <- function(x = matrix()){
        matr <- NULL
        setmatrix <- function(y) {
                x <<- y
                matr <<- NULL
        }
        getmatrix <- function() x
        setinv <- function (inv) matr <<- inv
        getinv <- function() matr
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinv = setinv, getinv = getinv)
        
}

## This function returns a matrix that is the inverse of 'x'
## If the inverse has already been calculated, it returns the cached matrix

cacheSolve <- function(x, ...) {
        
        
        matr <- x$getinv()
        if(!is.null(matr)){
                message("getting cached data")
                return(matr)
        }
        data <- x$getmatrix()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        
}
