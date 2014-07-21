## The pair of functions cache the inverse of a matrix so
##      the inverse operation is not repeated if the matrix
##      has not changed.

## makeCacheMatirx creates a special "matrix", which is really a list
##      containing a function to
##      1. set the values of the matrix, set
##      2. get the valuex of the matrix, get
##      3. set the values of the inverse, setinverse
##      4. get the values of the inverse, getinverse

## Assumption: The matrix supplied is invertible.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve calculates the inverse of the special "matrix" created
##      with the above function. However, it first checks to see if
##      the inverse has already been calculated and the matrix has not
##      changed. If so, it gets the inverse from the cache and
##      skips the computation. Otherwise, it calculates
##      the inverse of the matrix with solve() and sets the value of the
##      invese in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m   
}

##  Sample tests:
##  source("cachematrix.R")
##  mat = makeCacheMatrix(matrix(c(1,3,5,7), nrow=2, ncol=2))
##  mat$get()         # Returns original matrix
##       [,1] [,2]
##  [1,]    1    5
##  [2,]    3    7
##  cacheSolve(mat)   # Computes, caches, and returns inverse matrix
##         [,1]   [,2]
##  [1,] -0.875  0.625
##  [2,]  0.375 -0.125
##  mat$getinverse()  # Returns inverse matrix
##         [,1]   [,2]
##  [1,] -0.875  0.625
##  [2,]  0.375 -0.125
##  cacheSolve(mat)   # Returns cached inverse matrix using previously
##                        #     computed inverse matrix
##  getting cached data
##         [,1]   [,2]
##  [1,] -0.875  0.625
##  [2,]  0.375 -0.125
##  mat$set(matrix(c(2,4,6,8), nrow=2, ncol=2)) # Modify existing matrix
##  cacheSolve(mat)   # Computes, caches, and returns new inverse matrix
##       [,1]  [,2]
##  [1,] -1.0  0.75
##  [2,]  0.5 -0.25
##  mat$get()         # Returns matrix
##       [,1] [,2]
##  [1,]    2    6
##  [2,]    4    8
##  mat$getinverse()  # Returns inverse matrix
##       [,1]  [,2]
##  [1,] -1.0  0.75
##  [2,]  0.5 -0.25
##  cacheSolve(mat)   # Returns cached inverse
##  getting cached data
##       [,1]  [,2]
##  [1,] -1.0  0.75
##  [2,]  0.5 -0.25