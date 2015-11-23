## makeCacheMatrix caching the inverse matricies in memory and returns 
## a list of set and get for initial and reverse matrix

makeCacheMatrix <- function(x = matrix()) {
    newm <- NULL
    set <- function(y){
        x <<- y
        newm <<- NULL
    }
    get <- function() x
    seti <- function(I) newm <<- I
    geti <- function() newm
    list(set=set,get=get,seti=seti,geti=geti)
}


## This function call has two steps: calculate the inverse matrix or
## if it exists - return that matrix from cache

cacheSolve <- function(x, ...) {
    newm <- x$geti()
    if(!is.null(newm)) {
        message("getting cached data")
        return(newm)
    }
    data <- x$get()
    newm <- solve(data)
    x$seti(newm)
    newm
}
