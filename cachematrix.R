## These formulae can create a Matrix, cache its inverse, and solve it
## by retrieving it from the cache.

## makeCacheMatrix function:
## create a Matrix object, solves its  inverse, and cache its result.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y)        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## cacheSolve function:
## Resolves the inverse of the matrix from makeCacheMatrix. If the inverse
## has been solved before, the function gets it from the cache and doesn't
## peform the calculation; if not, computes the inverse and put it in the 
## cache.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m))         {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
## end
