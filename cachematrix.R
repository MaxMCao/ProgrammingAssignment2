## This is the second submission of this project. 
## The fisrt submission did not include the SHA-1 hash for the comments.

##  These functions cache the inverse of a matrix

## This function creates the matrix that can catche the inverse

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


## This function computes the inverse of the created matrix

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
