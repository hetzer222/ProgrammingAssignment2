## makeCacheMatrix generates a  matrix which also has a space 
## to hold its inverse.
## cacheSolve is analogous to solve, it generates the inverse of
##a cacheMatrix and saves the inverse matrix.

## makeCacheMatrix takes a matrix as its argument and
## returns a list containing the matrix, the inverse
## matrix(initialized as null until calculated with 
## cacheSolve) and the functions setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #the inverse matrix will be saved here by cacheSolve
  set <- function(y) {
    x <<- y
    m <<- NULL #if the matrix changed, invalidate the cached matrix
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,#return a list of attributes
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes a cacheMatrix as its argument and
## returns its inverse. The inverse matrix is saved
## as the attribute m of the cacheMatrix and can be 
## retrieved with the getinverse() attribute of the
## cachematrix. 
## Calling cacheSolve again will return the saved 
## inverse instead of recalculating the whole matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() #get the cached inverse matrix from the cacheMatrix
  if(!is.null(m)) { #if the inverse matrix has already exists
    message("getting cached data")
    return(m) #return the inverse matrix that we've saved
  }#if the inverse matrix wasn't saved already, we need to calculate it
  data <- x$get() #get the matrix
  m <- solve(data, ...) #calculate the inverse
  x$setinverse(m) #cache it in the cachematrix
  m #return the inverse
}
