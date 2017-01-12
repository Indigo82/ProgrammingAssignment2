## The functions calculates the inverse of a given matix. If the inverse is already calculated and cached, 
## the computation is skipped and the cached value is retrieved

## generate a object containing a list of functions with setter and getters
## these are not used the first time when initializing
## the function also assigns the initial values for x and m
makeCachematrix <- function(x = matrix()){
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve        
        getinverse <- function() m
        list(set=set, get=get, 
             setinverse=setinverse,
             getinverse=getinverse)
}

## the function return the inverse of a matrix. The x argument is the object from the makeCachematrix.
## the function checks if the value is already cached and get this value instead of computing it
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
 
mymatrix <- makeCachematrix(matrix(rnorm(25), nrow=5, ncol=5))
cacheSolve(mymatrix)
