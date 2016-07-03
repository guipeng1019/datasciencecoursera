## 2 functions, makeCacheMatrix & cacheSolve, used together to calculate matrix inversion.


## Testing code for peers: 
##    mat <-  cbind(c(1,1,1), c(1,1,0),c(1,0,0))  ##define the matrix  
##    t <- makeCacheMatrix(mat)
##    cacheSolve(t)


## makeCacheMatrix - to "define" operations later used in 2nd function    

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL

  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x 
  setInversion <- function(Inversion) Inv <<- Inversion 
  getInversion <- function() Inv  
  list(set = set, get = get,   
       setInversion = setInversion,   
       getInversion = getInversion)   
}


## cacheSolve - check the Inversion result from makeCacheMatrix. 
## If it is in cache get it from cache, otherwise calculate it. 

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInversion()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInversion(Inv)
  Inv
  
  }
