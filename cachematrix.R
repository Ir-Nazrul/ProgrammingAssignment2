## Assignment: Caching the Inverse of a Matrix
## The following functions is created to show that the inverse if a matrix can be cached

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        obj_inv <- NULL
        obj_init <- function(A) {
                x <<- A
                obj_inv <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setInverse <- function(inverse) {
                obj_inv <<- inverse
        }
        
        getInverse <- function() {
                obj_inv
        }
        
        list(obj_init = obj_init, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        obj_inv <- x$getInverse()
        
        if (!is.null(obj_inv)) {
                message("Display cached matrix")
                
                return(obj_inv)
        }
        
        mat <- x$get()
        obj_inv <- solve(mat, ...)
        x$setInverse(obj_inv)
        
        obj_inv
}
