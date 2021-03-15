## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(solve) inv <<- solve
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
    
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
    
    ## Return a matrix that is the inverse of 'x'
}


## Main 
x <- matrix(1:4, nrow = 2, ncol = 2)
list1 <- makeCacheMatrix(x)
inv1 <- cacheSolve(list1)
inv2 <- cacheSolve(list1)
