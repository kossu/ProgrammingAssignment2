## makeCachematrix and cacheSolve will calculate and cache the inverse of a 
## square matrix

## makeCachematrix assigns (input) data and functions (to get and set matrix inverse) 
## as a list to the parent environment allowing $ calls to functions.  

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL #initialize x & m
    
    set <- function(y) {
        x <<- y #assigns input (y) to x in parent env.
        m <<- NULL  #assigns null to m in parent env.
    }
    
    get <- function() x
    setinv <- function(solve) m <<- solve #assigns inv to m in parent env.
    getinv <- function() m
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv) #assigns functions as elements of list to parent env.. allows $ extracts
}

## cache Solve checks if the matrix inverse is already calculated and in cache
## if not it will calculate it and store it in cache

cacheSolve <- function(x, ...) {
    #if valid cached result, use that
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #if not, calculate (&set using $setmean) value 
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}