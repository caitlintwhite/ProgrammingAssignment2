## The two functions below, makeCacheMatrix() and cacheSolve(), intake an invertible matrix object 
## and store and return the inverse of that object respectively. 
## Because computing an inverse matrix can be CPU intensive, these two functions are helpful for retrieving information 
## at a later time when we might want it but do not want to recalculate the inverse.
 

## The fuction makeCacheMatrix() creates a special "matrix" object that can cache its inverse. This function
## return a list of 4 elements, which `cacheSolve` will call upon to return the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y = matrix()) {
                x <<- y
                m <<- NULL
                }
        get <- function() x
        set_inverse <- function(solve) m <<- solve
        get_inverse <- function() m
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## The function `cacheSolve` computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
## It does this by first checking if the inverse has already been cached.
## If the inverse has already been cached (and the matrix has not changed), then `cacheSolve` will return a 
## message indicating it is retrieving the cached data and will return the inverse from the cache.
## If the inverse matrix has not been cached, `cacheSolve` will retrieve the special matrix from `makeCacheMatrix`
## and calculate and return the inverse of the special matrix. It will set the value of the inverse via the set_inverse function in
## `makeCacheMatrix`

cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}
