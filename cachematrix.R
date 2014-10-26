## Write a short comment describing this function

## makeCacheMatrix function creates list to store a functions to:
## - set the Matrix
## - get the Matrix
## - set the inverse of the Matrix
## - get the inverse of the Matrix
makeCacheMatrix <- function(x = matrix()) {
    
    ## init variable m
    m <- NULL
    
    ## internal function that stores a Matrix
    setMtx <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## internal function that returns a Matrix 
    getMtx <- function() x
    
    ## internal function that stores the inverse of a Matrix
    setInvMtx <- function(solve) m <<- solve
    ## internal function that returns the inverse of a Matrix
    getInvMtx <- function() m
    
    ## create list to store above functions
    list(setMtx = setMtx,
         getMtx = getMtx,
         setInvMtx = setInvMtx,
         getInvMtx = getInvMtx)
}


## Write a short comment describing this function

## cacheSolve function calculates the inverse of the Matrix created with
## the above function.
## It first checks to see if the inverse of the Matrix has already been 
## calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse in ## the cache via the setInvMtx function.
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInvMtx()
    
    ## check to see if the innverse of the Matrix has already been calculated
    if(!is.null(m)) {
        ## if so, get the inverse from the cache...
        message("getting cached inverse of Matrix")
        ## ... return cached inverse of Matrix and skip the computation
        return(m)
    }
    
    ## otherwise get the Matrix and ...
    data <- x$getMtx()
    ## ... calculate the inverse of the data ...
    m <- try(solve(data, ...)) 
    ## store the inverse in the cache
    x$setInvMtx(m)
    m
}
