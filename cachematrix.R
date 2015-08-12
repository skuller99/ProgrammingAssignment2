
## This function initializes the special "matrix" which is a list. Its
## functions are: 1. set - set the matrix value 2. get - retrieve the value of
## the matrix 3. setinverse - set the inverse of the matrix 4. get the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


## This function calculates and returns the inverse of the special matrix 
## created by the function above. It checks whether the inverse has already
## been calculated, if yes prints "getting cached data" and returns the 
##same value, if not calculates, prints "First time" and returns the value.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        print("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    print("First time")
    m
}

