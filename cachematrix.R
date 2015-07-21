## Function creates a list of 4 other function (set,get,setInv and getInv)
## set stores the matrix in cache, get recalls the matrix
## setInv and getInv store and get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
m<-NULL  ## inversion results are going to be stored here
set <- function(y) {
  x <<- y
  m <<- NULL 
}

get <- function() x ## returns the original matrix
setInv <- function(inv) m <<- inv ## sets the inversed matrix
getInv <- function() m ## returns the inverted matrix

## a list that contains set,get,setInv,getInv functions, so that we can use
## change original matrix to the inverted one
list(set = set, get = get,
     setInv = setInv,
     getInv = getInv)
}


## This function calculates the inverse matrix of the makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getInv()   ## get the inversed matrix from object x
  if(!is.null(m)) { ## if loop to return calculated inversion
    message("getting cached data") ## checks if calculation was done before
    return(m)                      ## and returns form cache
  }
  data <- x$get()  ## if not found in cache gets the matrix 
  m <- solve(data)  ## and calcuates
  x$setInv(m)       ## we then set it to the object
  m                 ## returna the  result
}
