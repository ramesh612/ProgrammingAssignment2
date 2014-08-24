## Memoization is an optimization technique used to speed up programs by storing
## the output of expensive computation and returning the pre-computed result
## when the same inputs are used by the calling code. In this program, we are
## using the scoping rules of R (lexical) to achieve this.

## makeCacheMatrix creates a special "matrix", which is really a list containing
## a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve calculates the inverse of the special "matrix" created with the
## above function. This function first checks to see if the inverse has already
## been calculated. If so, it `get`s the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the data and sets the
## value of the inverse in the cache via the `setInverse` function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
