## Put comments here that give an overall description of what your
## functions do

## This function caches a matrix and its inverse

makeCacheMatrix <- function(x = matrix ()) { 
  i<-NULL 
  set <- function(y){
    y<<-x ##set stuff
    m<<- NULL
  }
  get<- function() x
  
  setinverse<-function(solve) i<<-solve ##set to the inverse of matrix.
  getinverse<- function() i
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This function returns the inverse of a matrix either 
##using solve or 
##retrieving the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse()
    if(!is.null(m)){ ##is m a cached value?
      message("getting cached data")
      return(m)
    }
    data<- x$get()   ##m is null, cache x
    m<-solve(data,...) ##solve to get inverse
    x$setinverse(m)   ##cache inverse m
    m               ##return inverse
    
  }
