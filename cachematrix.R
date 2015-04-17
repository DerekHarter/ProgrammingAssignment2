## Author: Derek Harter
## Date  : April 17, 2015
## Assig : Data Science, R Programming: Programming Assignment 2
##
## Desc  : Calculate matrix inversions and cache results.  
##         Matrix inversions can be costly, and if the results of
##         an inversion are needed several times, it makes sense to
##         perform the inversion once and reuse the results.  The
##         helper functions in file support creating a matrix
##         object wrapped with helper context (as a list).  When the 
##         inverse of the matrix object is needed, it is calculated
##         and the result saved.  Subsequent calls for the inverse
##         are simply retrieved from the cache without the need to
##         recompute.
##
##  The following example shows how to use these functions:
##
## > m <- matrix(c(2, 1, 4, 1, 3, 4, -1, -1, 1, -4, 1, 5, 2, -2, 1, 3), 4, 4)
## > mcache = makeCacheMatrix(m)
## > cacheSolve(mcache)
## [,1]       [,2]        [,3]       [,4]
## [1,] -0.11764706  0.1764706  0.23529412  0.1176471
## [2,] -0.05882353  0.3382353 -0.13235294  0.3088235
## [3,] -0.58823529  0.6323529 -0.07352941  0.8382353
## [4,]  1.00000000 -1.0000000  0.00000000 -1.0000000
## > cacheSolve(mcache)
## getting cached data
## [,1]       [,2]        [,3]       [,4]
## [1,] -0.11764706  0.1764706  0.23529412  0.1176471
## [2,] -0.05882353  0.3382353 -0.13235294  0.3088235
## [3,] -0.58823529  0.6323529 -0.07352941  0.8382353
## [4,]  1.00000000 -1.0000000  0.00000000 -1.0000000


makeCacheMatrix <- function(x = matrix()) {
    # Create object that encloses matrix and context to cache inversion.
    #
    # Args:
    #   x: The original matrix, or an empty matrix if not provided.
    #
    # Returns:
    #   A list that holds the matrix and helper functions used to 
    #   perform matrix inversion and caching of results.
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    # Inverse of matrix, cached result is returned if available,
    # otherwise we compute inverse and save result in cache.
    #
    # Args:
    #  x: A cached matrix object, created with makeCacheMatrix() function.
    #
    # Returns:
    #  A matrix that is the inverse of given matrix x.
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
