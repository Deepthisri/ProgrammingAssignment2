
## makeCacheMatrix is a function that takes a matrix argument and is calling a set of other functions 
## and returns the lists of the values stored in the defined functions.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL ##initializing variable m  to null
    ##set function where the value will be chached when the matrix is 1st created and any changes 
    ## made to the chached matrix
    set <- function(y) {
        x <<- y
        ## change the value of inverse of the matrix incase the matrix is     changed.
        m <<- NULL
    }
    ## get returns the value of the matrix passed
    get <- function() x
    #calculates the inverse of non-singular matrix via the solve function
    setinverse <- function(solve) m <<- solve
    ## getinverse returns inverse matrix stored in m
    getinverse <- function() m
    ## returns the functions called in makeChacheMatrix as a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## ChacheSolve gets chased matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ##if the matrix already exits then gets it from the chached data
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #if m in which inverse of matrix is store is null then  it is calculated and then retrieved.
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
