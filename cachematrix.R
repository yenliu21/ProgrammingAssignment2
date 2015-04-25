## Two functions are created to calcualte the inverse of the input matrix

## The function "makeCacheMatrix" has one argument "x" and outputs a list of 4 functions:
## setmatrix, getmatrix, setinverse, and getinverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL 
    
    setmatrix <- function(y) {
        x <<- y 
        m <<- NULL 
    }
    
    getmatrix <- function() x 
    
    setinverse <- function(inverse) m <<- inverse 

    getinverse <- function() m 
    
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse) 
}


## The function "cacheSolve" first checks if the inverse exists
## 1. if the inverse exists, the function returns this inverse
## 2. if the inverse does not exist, the function calculates the inverse of the input matrix then returns the inverse

cacheSolve <- function(x, ...) {    
    m <- x$getinverse()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$getmatrix()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}