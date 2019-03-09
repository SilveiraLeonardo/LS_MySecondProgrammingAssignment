## The function makeCacheMatrix creates a special type of matrix, that 
# in conjunction with the functio cacheSolve, can cache the value of its inverse
# so that the program does not need to calculate it again everytime it is called

makeCacheMatrix <- function(x = matrix()) {

    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    #the inverse will only have value after the cacheSolve is used
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function calculates the inverse of the especial type of matrix returned by
# the fucntion makeCacheMatrix. If the inverse has already been computed, this function
# will retrieve its value, so that the calcularion will not be done unecessarily.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    s <- x$getinverse()
    
    #in this part it checks if the mean was already calculated
    #if it was, the function just recover the calculated value
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
    
}
