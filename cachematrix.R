## These two functions allow a large matrix to be cached in
## a separate environment so that its inverse can be calculated
## 
## The inverse of the matrix will be saved in the separate
## environtment so it doesn't have to be computed when called

## The function makeCacheMatrix will cache a matrix in
## a separate environment
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }

    get <- function() x
    set_inv <- function(solve) m <<- solve
    get_inv <- function() m
    list(set = set, get = get,
         set_inv = set_inv, get_inv = get_inv)
}


## The function cacheSolve will calculate the inverse
## of the cashed matrix

cacheSolve <- function(x, ...) {
  m <- x$get_inv()
    if(!is.null(m)) {                             ## If the inverse has already been
              message("getting cached data")      ## calculated, it gets the inverse from
              return(m)                           ## the cache, skips the computation and issues a message
    }
    data <- x$get()
        m <- solve(data,...)                      ## If the inverse has not been calculated
         x$set_inv(m)                             ## then this will invert the cached matrix
    m
        
}   ##Returns the inverse of the cached matrix

