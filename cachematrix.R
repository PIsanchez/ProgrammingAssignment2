## The first function makeCacheMatrix creates a new "special matrix"
## its especial because it has an atribute called inverse which stores 
## the inverse matrix of the special matrix

## The second function cacheSolve calculates the inverse of a Special Matrix created
## with makeCacheMatrix if it doesn't have inverse yet.
##We asume that all matrix we input are invertible.

##Sorry if I made a sintax or grammar mistake, english is not my native language



## By default the inverse matrix is not calculated, so we set it to be NULL
## Also, when we re-set x, the inverse matrix changes, so again it isn't calculated and we set it NULL

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL 
  set <- function(y) {
    x <<- y     
    inverse <<- NULL  
  }
  get <- function() x
  setinverse <- function(I) inverse <<- I 
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      invisible(inverse)
}


##Example
##Using the functions:
A = matrix(c(2, 4, 3, 1, 5, 7, 1, 2, 3), nrow=3, ncol=3, byrow = TRUE)
#just checking if A is a matrix, numeric and invertible
is.matrix(A)
is.numeric(A)
print("A, is invertible?")
det(A)!=0 
#creates special matrix SpecialMatrix
SpecialMatrix=makeCacheMatrix(A)
SpecialMatrix$get()
SpecialMatrix$getinverse()# it should be NULL, cause we haven't used cacheSolve yet

cacheSolve(SpecialMatrix)
SpecialMatrix$getinverse()# now it should return the inverse matrix of the Special Matrix

round(SpecialMatrix$get()%*%SpecialMatrix$getinverse())#we check if we have the correct solution

#
SpecialMatrix$setinverse(A)#this line is not the correct inverse, is just for checking...
#... if the chacheSolve works ok when we already have an inverse
cacheSolve(SpecialMatrix)
SpecialMatrix$getinverse()
#As we see cacheSolve doesn't recalculate the inverse, as expected.

#So, everything is working properly
