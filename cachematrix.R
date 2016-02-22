## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
            x <<- y
            # "<<-" assigns the value to an object in a different environment
            inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
               message("The inverse of the matrix has been calculated!")
               return(inv)
        }
        result <- x$get()
        inv <- solve(result,...)
        x$setinverse(inv)
        inv
}
