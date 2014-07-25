## "cachematrix" has two functions - "makeCacheMatrix" and 
## "cacheSolve", that togather cache the inverse of a matrix.
## "makeCacheMatrix" function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
       x <<- y
       i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<-solve
    getinverse <- function() i
    list(set =set, get =get, 
         setinverse = setinverse,
         getinverse = getinverse)  
}


## "cacheSolve" function computes the inverse of the special "matrix" 
## returned by "makeCacheMatrix" function. "cacheSolve" function 
## will retrieve the inverse from cache, if its already calculated and if 
## the matrix hasn't changed. - nkandasa, 07/23/2014

cacheSolve <- function(x, ...) {
        i <-x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        mat <- x$get()
        i <-solve(mat, ...)
        x$setinverse(i)
        i
}