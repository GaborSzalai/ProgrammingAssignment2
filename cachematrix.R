##  The two functions below enable you to store the inverse of a given matrix in a cache.
##  When calculating the inverse of this cached matrix, the cached state is returned instead of
### re-running the inverse calculation again.


##  This funtion creates a special 'matrix' that is essentially a list of functions for
### setting and reading a matrix and its inverse state. 

makeCacheMatrix <- function(x = matrix()) {
        
        inv<- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)


}


##  This function produces the inverse of the 'matrix' produced with the makeCacheMatrix function.
### If the inverse is already in the cache, the function returns the cached value. If not, it 
#### calculates the mean and stores it in the matrix cache.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
 