## Functions for caching the inverse of a matrix

## Function makeCacheMatrix:
## Defines functions used in cacheSolve
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL   # initialize m
    
    # function "set": sets the values of x and m
    set <- function(y) {
        x <<- y  
        m <<- NULL
    }
    
    # function "get": gets value of x
    get <- function() x
    
    # function "setinverse": saves inverse as m in cache
    setinverse <- function(inverse) m <<- inverse
    
    # function "getinverse": gets value of m
    getinverse <- function() m
    
    # create list of the four functions defined above (named to function name)
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Function cacheSolve:
## Gets inverse from cache if exists, else calculates and saves inverse to cache
cacheSolve <- function(x, ...) {
    m <- x$getinverse()    # set m from cache
    
    # if inverse exists in cache: return inverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m) 
    }
    
    # else calculates inverse and saves it to cache before returning it
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
