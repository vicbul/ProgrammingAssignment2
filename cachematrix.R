## Following the directrices provided by the example used in the Programming Assignment 2: Lexical Scoping,
## I worte a pair of functions that returns the inverse of a given matrix, and cache the result for a
## later use, avoiding redundant calculations.

##makeCacheMatrix function takes a matrix as an imput, and returns a list containing functions to:
##1-.Set the value of the matrix
##2-.Get the value of the vector
##3-.Set the value of the inverted matrix
##4-.Get the value of the inverted matrix

makeCacheMatrix <- function (x = matrix()) {
    ##initializing the variable that will store the inverted matrix (m)
    m <- NULL
    #set function takes a new input matrix and resets previous cached inverted matrix (m) back to NULL.
    set <- function (y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverted) m <<- inverted
    getinv <- function() m
    list (set = set, get= get, setinv = setinv, getinv = getinv)
}


##cachesolve function takes the output of makeCahceMatrix and checks whether or not inverted matrix (m)
##have been already cached, otherwise it calculates and stores it before returning the result

cachesolve <- function (x,...) {
    m <- x$getinv()
    if (!is.null(m)) {
        message("Returning cached inverted matrix")
        return(m)
    } else {
        matrix <- x$get()
        m <- solve(matrix)
        x$setinv(m)
    }
    return(m)
}
