## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather 
## than computing it repeatedly 

## The function makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse.
## It is really a list containing a function to
##
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        ## <<- operator which can be used to assign a value to an object
        ## in an environment that is different from the current environment
        x <<- y
        inv <<- NULL
    }
    get <- function() return(x)
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() return(inv)
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)

}

## The function cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    ## returns the inverse if available
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## sets the inverse if not available
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    return(inv)
    
}

# short script for testing the functions
cat("\n",
    "#######################################################################\n",
    "## short script for testing the functions\n",
    "#######################################################################\n",
    "\n", sep="")

m <- makeCacheMatrix(matrix(c(1:4),2,2))
cat("m$get=\n")
print(m$get())
cat("\nm$getinv=\n")
print(m$getinv())

cat("\ncalling cacheSolve...\n")
minv <- cacheSolve(m)
cat("\nm$getinv=\n")
print(m$getinv())

cat("\ncalling cacheSolve...\n")
minv <- cacheSolve(m)

