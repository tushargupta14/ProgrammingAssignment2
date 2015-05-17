



makeCacheMatrix <- function(x = matrix()) {

  inverse<-NULL
  get<-function() 
    x
  set<-function(y)
  {
    x<<-y
    inverse<<-NULL
  }
  
  getinverse<-function()
  {
    inverse
  }
  
  setinverse<-function(inver)
  {
    inverse<<-inver
  }
  list(get=get,set=set,getinverse=getinverse,setinverse=setinverse)
}


## Write a short comment describing this function

  cacheSolve <- function(x, ...) {
    s <- x$getinverse()
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
  }
