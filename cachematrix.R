##############################################################################
## Here define two functions: makeCacheMatrix and cacheSolve                ##
##   makeCacheMatrix would take a matrix as input, and have a inverseMatrix ##
##       stored inside it.                                                  ##
##   cacheSolve caculate the inverse matrix of the original matrix and      ##
##       store it in the object, if no inverseMatrix is found. It would     ##
##       return the inverseMatrix if it already caculated it.               ##
##                                                by PythonShell            ##
##                                                2015.03.16                ##
##############################################################################

##############################################################################
## Function: makeCacheMatrix                                                ##
## Input:    A default matrix data                                          ##
## Output:   A modified matrix which would store its inverse Matrix in it   ##
##############################################################################

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <- NULL
    }
    get <- function() x
    setInverseMatrix <- function(m) {
        inverseMatrix <<- m
    }
    getInverseMatrix <- function() inverseMatrix
    list(set = set, get = get,
        setInverseMatrix = setInverseMatrix,
        getInverseMatrix = getInverseMatrix)
}

##############################################################################
## Function: cacheSolve                                                     ##
## Input:    a CacheMatrix                                                  ##
## Output:   the Inverse Matrix of input, and cache the inverse matrix      ##
##               in the CacheMatrix                                         ##
##############################################################################

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverseMatrix()
    if(!is.null(m)) {
        message("getting the inverse matrix")
        return(m)
    }
    data <- x$get()
    ## do the inverse operation
    m <- solve(data)
    x$setInverseMatrix(m)
    m
}
