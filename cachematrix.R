## These two functions can be used to calculate inverse of matrix
## When inverse of one matrix is calculated, that result is saved in cache
## so the next time when you want to get that inverse it's not calculated again
## but that result is taken from cache

## Function makeCacheMatrix is made of 4 functions
## that are used to cache the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
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


## Function cacheSolve checks if there is inerse of matrix in cache
## If there is inverse in cache it returns it
## If not it calculates invese using solve fuction
## Calculated result is first saved in cache using 
## result of funcion makeCacheMatrix, then returned

cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    matrix <- x$get()
    
    inv <- solve(matrix)
    
    x$setinv(inv)
    
    inv
    
}
