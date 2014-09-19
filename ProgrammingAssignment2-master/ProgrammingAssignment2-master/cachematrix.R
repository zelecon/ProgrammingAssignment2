## Put comments here that give an overall description of what your
## functions do

## Function creats a list object with 4 entries. Saves the matrix and its inverse passed
## to it by cacheSolve, in a unique environment. If the same makeCacheMatrix list object is 
## passed to it by cacheSolve function a non-NULL inverse matrix will be returned to
## invm in cacheSolve function and it will be the return value of cacheSolve function.
## If a new makeCacheMatrix list object is passed to it by the cacheSolve function 

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinv <- function(slvm) invm <<- slvm
    getinv <- function() invm
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## cacheSolve takes a makeCacheMatrix object as its argument. Checks if the passed object's
## inverse matrix has been evaluated previously by accessing the getinv element of the passed
## object and checking if it is NULL. Function reutrns the cached value of getinv if it is not
## NULL otherwise the inverse of the makeCacheMatrix object is calculated and the list element
## getinv is set using setinv function of the passed object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invm <- x$getinv()
    if(!is.null(invm)) {
        message("getting cached inverse matrix")
        return(invm)
    }
    mat <- x$get()
    invm <- solve(mat, ...)
    x$setinv(invm)
    invm
}
