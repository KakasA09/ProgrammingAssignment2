# The makeCacheMatrix function creates a special "matrix" object that can cache its inverse
# When testing the code, please ensure the matrix object you create is a square matrix


# There are 3 main sub-functions below: the "get" function returns the square matrix
# we specified earlier as part of the makeCacheMatrix function:
# { i.e. mymatrix <- makeCacheMatrix(matrix(1:4, 2,2)) } 
# the "setinverse" function solves the inverse of our matrix, and assigns it to the "m" variable
# the "getinverse" function returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
     x <<- y
     m <<- NULL
   }
  
  get <- function() x
  setinverse <- function(inverse) m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)

}

# The cacheSolve function (i.e. cacheSolve(mymatrix) from the prior example) returns
# the inverse of our matrix using the cache that we assigned (if the matrix is the same)
# This function takes our previous "getinverse" function, and assigns it to "m"...
#...which is our cached inverse matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
  
}



