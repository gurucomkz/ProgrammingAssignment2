## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix stores given matrix as special object
## with some sort of getter and setter for the value itself
## and inverted matrix
##
## checks whether input data has class matrix. throws error otherwise

makeCacheMatrix <- function(x = matrix()) {
    check <- function(x){ #to check input is type of matrix
        if(!is.matrix(x))
            stop("Argument must be a matrix")
    }
    check(x)
    solved <- NULL
    get <- function() x
    set <- function(y = matrix()){
        check(x)
        x <<- y
        solved <<- NULL
    }
    getsolve <- function() solved
    setsolve <- function(dummy){
        check(dummy)
        solved <<- dummy
    }
    return (list(
        get = get, set = set,
        getsolve = getsolve, setsolve = setsolve
    ))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    checkNconvert <- function(candidate){
        ## We check for object's interface now
        ## We do not take any extensions possible extensions
        ## of valid object into account
        validObject <- makeCacheMatrix()
        if(!identical(sapply(validObject, class), 
                      sapply(candidate, class))){
            if(is.matrix(candidate))
            {
                warning("Plain matrix given. Converting to cacheable with makeCacheMatrix")
                return (makeCacheMatrix(candidate))
            }
            stop("Argument must be a matrix of a product of makeCacheMatrix()")
        }
        return (candidate)
    }
    
    ## check x for validity and convert to valid object if possible 
    obj <- checkNconvert(x)
    ## Return a matrix that is the inverse of 'x'
    m <- obj$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- obj$get()
    m <- solve(data, ...)
    obj$setsolve(m)
    m
}
