## This function create special object for using in calculation the Inverse of a Matrix
## The object store original and inverse matrix and 4 fucntions: getter and setter for each matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## This function using directly in calculation the Inverse of a Matrix.
## It return inverse matrix with using special object created by makeCacheMatrix.
## If caching matrix is stored in previous calculations then this matrix returned.
## Otherwise, inverse matrix is calculated and stored in cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}

