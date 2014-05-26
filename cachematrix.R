## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { ##Create function to set value of matrix
        x <<- y 
        inv <<- NULL
    }
    get <- function() x ##Create function to get value of matrix
    setInverse <- function(inverse) inv <<- inverse ##Create function to set value of inverse of matrix
    getInverse <- function() inv ##Create function to get value of inverse of matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ##Return a lis wih defined functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) { ##check if computed inverse of matrix is already cached
        message("getting cached data")
        return(inv) ##get cached inverse of matrix
    }
    data <- x$get() ##if not yet cashed
    inv <- solve(data, ...) ##compute inverse
    x$setInverse(inv)   ##Cashe inverse
    inv
}
