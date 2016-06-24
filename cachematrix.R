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


## cacheSolve:
## accepts a product of makeCacheMatrix (or list with similar contents)
## and returns inverse matrix of it. Function checks whether cached inverse
## matrix already exists in the given object. If so, cached result is returned.
## Otherwise newly computed inverse matrix is stored into the object.
##
## If x doesn't look like a product of makeCacheMatrix(), error is thrown

cacheSolve <- function(x, ...) {
    check <- function(candidate){
        ## We check for object's interface now
        ## We do not take any extensions possible extensions
        ## of valid object into account
        validObject <- makeCacheMatrix()
        if(!identical(sapply(validObject, class), 
                      sapply(candidate, class)))
            stop("x must be a product of makeCacheMatrix()")
    }
    
    ## check x for validity
    check(x)
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
