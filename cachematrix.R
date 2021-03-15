## Two functions are defined within this script: makeCacheMatrix()" and "cacheSolve()". After those definitions, a 2-by-2 matrix is defined and passed to the first function. The output of this function (a list containing pointers to functions defined within "makeCacheMatrix()") is passed to the second function to compute the inverse of the matrix. The second call to "cacheSolve()" with the same input argument as before returns that the function is retrieven the cached inverse computation.

## makeCacheMatrix() builds a list of pointers to functions that stores the matrix passed to it and initializes the corresponding inverse matrix as NULL. 

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

## cacheSolve() computes the inverse of the matrix stored in the list defined in the previous call the function makeCacheMatrix(). To do so, it ask if the inverse matrix is already computed, returning in that case a message that the corresponding expresion is cached. If not, it compute the inverse using the R built-in solve() function.

cacheSolve <- function(x, ...) {
    
	## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
    
}


## Main 
x <- matrix(1:4, nrow = 2, ncol = 2)
list1 <- makeCacheMatrix(x)
inv1 <- cacheSolve(list1)
inv2 <- cacheSolve(list1)
