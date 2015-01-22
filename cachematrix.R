## These two functions serve to cache the result of a matrix inversion.
## If a matrix has been inverted before, this will save time and resources 
## by recalling the stored value. 

makeCacheMatrix <- function(x = matrix()) {
        # makeCachedMatrix take an matrix and precalculate the inverted 
        # matrix for later use.
        # input is a matrix
        # return a vector that is a list of functions
        #    set the value of the matrix
        #    get the value of the matrix
        #    set the value of the inverted matrix
        #    get the value of the inverted matrix
        I <- NULL
        set <- function(y){
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) I <<- Inv
        getInv <- function() I
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}



cacheSolve <- function(x, ...) {
        # cacheSolve takes in the input from the vector created by makeCacheMatrix 
        # and calculate the invert matrix.
        # If the inverted matrix has been calculated, it returns the stored value
        # otherwise, it calculate the inverted matrix and assign the value to be stored.
        # Return a matrix that is the inverse of 'x'
        I <- x$getInv()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setInv(I)
        I
}
