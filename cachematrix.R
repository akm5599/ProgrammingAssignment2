# FUNCTION DESCRIPTIONS:

#    makeCacheMatrix

##   1. Create the function "makeCacheMatrix" with the argument x, which is a
##      an empty matrix. Also, the object m, which is in the makeCacheMatrix
##      environment, is set to NULL.

##   2. Create the function "set" with the argument y. The value of y is 
##      assigned to the object located in the parent environment (makeCacheMatrix)
##      called x. The object m's value is also reset to NULL.

##   3. Create the function "get" that uses the object x from the parent
##      environment.

##   4. Create the function "setinverse" with the argument inverse. Set the value 
##      of the object m (located in the parent environment) to the input called 
##      "inverse" in the function "setinverse."

##   5. Create the function "getinverse" that uses the object m from the parent
##      environment.

##   6. Name each of the four functions created under 
##      makeCacheMatrix as the names of their corresponding function (i.e. the 
##      function setinverse is given the name "setinverse"). This is done via the
##      list feature.


#    cacheSolve

##   1. Creat the function "cacheSolve" with the input argument x, and the option 
##      to add additional arguments later on.

##   2. The function tries to call the getinverse function on x and assigns it to
##      m. Then the function checks to see if this object m is NULL or not. If it 
##      has a value other than NULL, then it returns a message stating that it will 
##      retrieve the cached data, and then it returns the value of m. If it is NULL,
##      the function gathers the input data from the input argument, solves for the 
##      inverse, sets the inverse to the object m, and returns the value of m.



## This function is used to establish the set, get, setinverse and getinverse 
## functions.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This function is used to perform the actual process of finding the inverse of x.

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

