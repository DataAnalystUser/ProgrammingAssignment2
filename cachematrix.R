## makeCacheMatrix creates a special matrix that can cache it's inverse.
## cacheSolve function computes the inverse of the matrix created by makeCacheMatrix
and will retrieve the inverese from the cache if it has already been solved.

makeCacheMatrix <- function(x = matrix()) {
            sol <- NULL                       ##Sets function to NULL
            set <- function(y) {             
                    x <<- y                   
                   inv<<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) sol <<- inverse
            getinverse <- function() sol
            list(set = set, get = get,        ## Creates a list
                 setinverse = setinverse,
                 getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
             sol <- x$getinverse()           ##Attempts to retrieve the inverse from cache
            if(!is.null(sol)) {              ##If the inverse has been created before, the
             message("getting cached data")  ##matrix will be retrieved from the cache and
             return(sol)                     ##returned.
            }
            data <- x$get()                  ##If it has not been cached
            sol <- solve(data, ...)          ##the inverse matrix function"solve" will
            x$setinverse(sol)                ##return the inverse matrix
            return(sol)        
}
