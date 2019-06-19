## This function creates a special matrix that can cache the mean of its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

x <- matrix(1:4, 2)
makeCacheMatrix(x)

## Depending on the value of 'x', calculate or retrieve mean of 'x'
cachemean <- function(x) {
    #this will get the value of 'getinverse' from list x, created above
    inv <- x$getinverse()
    
    #if x$getinverse produces a null value, meaning the inverse hasn't been cached yet, the following will execute 
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    #create object containing the value of 'get' from list 'x'
    data <- x$get()
    
    #get the mean of the inverse of the matrix x'
    inv <- x$setinverse(data)
    mean(inv)
    
}
cachemean(makeCacheMatrix(x))

