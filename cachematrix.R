## makeCacheMatrix and cacheSolve are two functions created to work in 
## conjuction to compute the value of inverse matrix, difference is if
## inverse matrix been already created then re computation will not be done
## the result will be fetched from cache instead

## makeCacheMatrix for 
## creating a special vector containing a function to
##1.  set the value of the vector matrix
##2.  get the value of the vector matrix
##3.  set the value of the inverse matrix
##4.  get the value of the inverse matrix
## 
## cacheSolve function computes the inverse matrix using the special vector
## created from previous function, however it first check that if inverse matrix
## already been calculated, if yes then it fetch the values from cache instead of
## recomputation as in usual case

makeCacheMatrix <- function(x = matrix()) {

  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


cacheSolve <- function(x,...) {

    s <- x$getsolve()  ## Assign Value: special list vector x for object getsolve   
   
    if(!is.null(s)) {  ## execute code if value is not null
        message("getting cached data")
       return(s)        ## Returning stored value from Cache and exit
    }
    
  data <- x$get() ## Assign Value: special list vector x for object get
  s <- solve(data,...) ## Calculating inverse of matrix
  x$setsolve(s)       ## Setting value in list vector x for object setsolve
  s    ## Returning Newly computed inverse matrix and exit
  
}

InvExm <- makeCacheMatrix(matrix(1:4,2,2))

cacheSolve(InvExm)

