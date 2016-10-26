
## Assignment 2, Week 3, r programming module

## These functions aim to save computation time 
## by not having to recalculate matrix inverses if
## already calculated.  Its based on code I don't 
## really understand.

## The first funtion takes the matrix for which you want
## to find the inverse, and creates a list that stores (sets)
## the inverse if it has been calculated, and allows the
## second function to retrieve this.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ## inverse set to null to start with
  set <- function(y) {
    x <<- y  ## assign y to x only in this fn env??
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## This next function retrives the inverse from the first 
## function and  provides it as "m" if not null.
## If the value is null, it solves for the
## inverse using r's "solve" function and then sets the
## inverse into the first function, so it can be 
## retrieved next time. Either way it returns the inverse.

## to use the fuunctions for a matrix "x", apply as
## cacheSolve(makeCacheMatix(x))

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m) ## puts the calculate value into 1st fn
  m
}