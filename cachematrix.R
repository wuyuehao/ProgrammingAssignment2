
## create a new matrix that could cache an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <-function(y){
       x<<- y
       m <<- NULL
    }
    set(x)
    get <- function() x
    setInverse <- function(inverse) m<<- inverse
    getInverse <- function() m
    list(set=set, get=get, getInverse = getInverse, setInverse = setInverse)
    
}


## get the Inverse matrix of matrix x. x must be generated from makeCacheMatrix function. 
## This function check whether there is a cached value and calculate 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)){
       message("getting cached data")
       return(m)
    }
    ## cache missing, calculate and put into cache.
    data <- x$get()
    print(data)
    m <- solve(data)
    x$setInverse(m)
    m
}
