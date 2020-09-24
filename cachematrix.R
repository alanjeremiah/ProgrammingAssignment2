## The below functions help to cache the inverse of a matrix 
## Matrix inverse is a costly computation, so when needed to find the inverse
## of the same matrix repeatedly, it is better to cache the inverse matrix, so when
## the same matrix is called to find inverse, instead of computing, the cached value
## of its inverse is returned

## The makeCacheMatrix function creates a special matrix object that can cache its
## inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(mat) {
                x <<- mat
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() {
                inv
        }
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## The cacheSolve function returns the inverse of the matrix in the special object 
## passed. If inverse exists in cache, return that value, else find the inverse, 
## store in cache and also return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
